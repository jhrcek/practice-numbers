port module Main exposing (main)

-- Portuguese number drill as a static web app.
--
-- Flow: Setup (mode, range, goal) -> Session (rounds) -> Results.
--   Listen: audio plays, user types the number, app judges it.
--   Speak:  number is shown, user says it aloud, plays the mp3 to
--           compare, then self-evaluates.
-- Audio playback goes through a port (see index.html); mp3s named
-- 0.mp3 .. 9999.mp3 live in the audio/ directory next to index.html.
--
-- Each answer is timed from question start until the user commits
-- (Check in listen mode, Hear it in speak mode); the results screen
-- shows the average over all attempts, correct and wrong alike.

import Browser
import Browser.Dom as Dom
import Browser.Events
import Html exposing (Html, button, div, h1, h2, input, label, p, section, span, text)
import Html.Attributes as A
import Html.Events as E
import Json.Decode as Decode
import Random
import Task
import Time


port playAudio : String -> Cmd msg


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( initModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { form : SetupForm
    , screen : Screen
    }


type Screen
    = Setup
    | InSession Session
    | Finished Session


type Mode
    = Listen
    | Speak


type GoalKind
    = ReachCorrect -- practice until N correct, mistakes are counted
    | FixedAttempts -- exactly N attempts, correct answers are counted


type alias SetupForm =
    { mode : Mode
    , minInput : String
    , maxInput : String
    , goalKind : GoalKind
    , goalInput : String
    , error : Maybe String
    }


type alias Session =
    { mode : Mode
    , rangeMin : Int
    , rangeMax : Int
    , goalKind : GoalKind
    , goalTarget : Int
    , correct : Int
    , wrong : Int
    , current : Int
    , phase : Phase
    , questionStart : Maybe Time.Posix
    , answerMillis : List Int
    }


type Phase
    = Loading
    | ListenAnswering String
    | ListenVerdict { given : String, wasCorrect : Bool }
    | SpeakThinking
    | SpeakEvaluating


initModel : Model
initModel =
    { form =
        { mode = Listen
        , minInput = "0"
        , maxInput = "99"
        , goalKind = ReachCorrect
        , goalInput = "10"
        , error = Nothing
        }
    , screen = Setup
    }



-- UPDATE


type Msg
    = NoOp
      -- setup screen
    | ModeChosen Mode
    | MinChanged String
    | MaxChanged String
    | PresetChosen Int Int
    | GoalKindChosen GoalKind
    | GoalChanged String
    | StartClicked
      -- session
    | GotNumber Int
    | GotStartTime Time.Posix
    | GotAnswerTime Time.Posix
    | AnswerChanged String
    | AnswerSubmitted
    | ReplayClicked
    | NextClicked
    | RevealClicked
    | SelfEvaluated Bool
      -- results screen
    | RestartClicked
      -- back to setup (results screen, exit button in session)
    | ChangeSettingsClicked


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ModeChosen m ->
            updateForm (\f -> { f | mode = m }) model

        MinChanged v ->
            updateForm (\f -> { f | minInput = v }) model

        MaxChanged v ->
            updateForm (\f -> { f | maxInput = v }) model

        PresetChosen lo hi ->
            updateForm (\f -> { f | minInput = String.fromInt lo, maxInput = String.fromInt hi }) model

        GoalKindChosen k ->
            updateForm (\f -> { f | goalKind = k }) model

        GoalChanged v ->
            updateForm (\f -> { f | goalInput = v }) model

        StartClicked ->
            case validate model.form of
                Ok session ->
                    ( { model | screen = InSession session }, roll session )

                Err e ->
                    updateForm (\f -> { f | error = Just e }) model

        GotNumber n ->
            withSession model
                (\s ->
                    let
                        s2 =
                            { s | current = n, phase = startPhase s.mode }
                    in
                    case s.mode of
                        Listen ->
                            ( InSession s2, Cmd.batch [ playAudio (mp3Url n), focus answerInputId, now GotStartTime ] )

                        Speak ->
                            ( InSession s2, Cmd.batch [ focus playButtonId, now GotStartTime ] )
                )

        GotStartTime t ->
            withSession model (\s -> ( InSession { s | questionStart = Just t }, Cmd.none ))

        GotAnswerTime t ->
            withSession model
                (\s ->
                    case s.questionStart of
                        Just start ->
                            ( InSession
                                { s
                                    | answerMillis = (Time.posixToMillis t - Time.posixToMillis start) :: s.answerMillis
                                    , questionStart = Nothing
                                }
                            , Cmd.none
                            )

                        Nothing ->
                            ( InSession s, Cmd.none )
                )

        AnswerChanged v ->
            withSession model
                (\s ->
                    case s.phase of
                        ListenAnswering _ ->
                            ( InSession { s | phase = ListenAnswering v }, Cmd.none )

                        _ ->
                            ( InSession s, Cmd.none )
                )

        AnswerSubmitted ->
            withSession model
                (\s ->
                    case s.phase of
                        ListenAnswering raw ->
                            let
                                answer =
                                    String.trim raw
                            in
                            if answer == "" then
                                ( InSession s, Cmd.none )

                            else
                                let
                                    good =
                                        String.toInt answer == Just s.current

                                    s2 =
                                        tally good { s | phase = ListenVerdict { given = answer, wasCorrect = good } }
                                in
                                ( InSession s2, Cmd.batch [ focus nextButtonId, now GotAnswerTime ] )

                        _ ->
                            ( InSession s, Cmd.none )
                )

        ReplayClicked ->
            withSession model (\s -> ( InSession s, playAudio (mp3Url s.current) ))

        NextClicked ->
            withSession model advance

        RevealClicked ->
            withSession model
                (\s ->
                    case s.phase of
                        SpeakThinking ->
                            ( InSession { s | phase = SpeakEvaluating }
                            , Cmd.batch [ playAudio (mp3Url s.current), now GotAnswerTime ]
                            )

                        _ ->
                            ( InSession s, Cmd.none )
                )

        SelfEvaluated good ->
            withSession model
                (\s ->
                    case s.phase of
                        SpeakEvaluating ->
                            advance (tally good s)

                        _ ->
                            ( InSession s, Cmd.none )
                )

        RestartClicked ->
            case model.screen of
                Finished s ->
                    let
                        fresh =
                            { s | correct = 0, wrong = 0, phase = Loading, questionStart = Nothing, answerMillis = [] }
                    in
                    ( { model | screen = InSession fresh }, roll fresh )

                _ ->
                    ( model, Cmd.none )

        ChangeSettingsClicked ->
            ( { model | screen = Setup }, Cmd.none )


updateForm : (SetupForm -> SetupForm) -> Model -> ( Model, Cmd Msg )
updateForm f model =
    ( { model | form = f model.form }, Cmd.none )


withSession : Model -> (Session -> ( Screen, Cmd Msg )) -> ( Model, Cmd Msg )
withSession model f =
    case model.screen of
        InSession s ->
            let
                ( screen, cmd ) =
                    f s
            in
            ( { model | screen = screen }, cmd )

        _ ->
            ( model, Cmd.none )


validate : SetupForm -> Result String Session
validate form =
    case ( String.toInt (String.trim form.minInput), String.toInt (String.trim form.maxInput), String.toInt (String.trim form.goalInput) ) of
        ( Just lo, Just hi, Just goal ) ->
            if lo < 0 || hi > 9999 then
                Err "Range must stay between 0 and 9999 (that's all the recordings we have)."

            else if lo > hi then
                Err "Range: 'from' must not exceed 'to'."

            else if goal < 1 then
                Err "The goal must be at least 1."

            else
                Ok
                    { mode = form.mode
                    , rangeMin = lo
                    , rangeMax = hi
                    , goalKind = form.goalKind
                    , goalTarget = goal
                    , correct = 0
                    , wrong = 0
                    , current = 0
                    , phase = Loading
                    , questionStart = Nothing
                    , answerMillis = []
                    }

        _ ->
            Err "Please fill in all fields with whole numbers."


startPhase : Mode -> Phase
startPhase mode =
    case mode of
        Listen ->
            ListenAnswering ""

        Speak ->
            SpeakThinking


roll : Session -> Cmd Msg
roll s =
    Random.generate GotNumber (Random.int s.rangeMin s.rangeMax)


tally : Bool -> Session -> Session
tally good s =
    if good then
        { s | correct = s.correct + 1 }

    else
        { s | wrong = s.wrong + 1 }


advance : Session -> ( Screen, Cmd Msg )
advance s =
    if sessionDone s then
        ( Finished s, Cmd.none )

    else
        ( InSession { s | phase = Loading }, roll s )


sessionDone : Session -> Bool
sessionDone s =
    case s.goalKind of
        ReachCorrect ->
            s.correct >= s.goalTarget

        FixedAttempts ->
            s.correct + s.wrong >= s.goalTarget


mp3Url : Int -> String
mp3Url n =
    "audio/" ++ String.fromInt n ++ ".mp3"


focus : String -> Cmd Msg
focus id =
    Task.attempt (\_ -> NoOp) (Dom.focus id)


now : (Time.Posix -> Msg) -> Cmd Msg
now toMsg =
    Task.perform toMsg Time.now


answerInputId : String
answerInputId =
    "answer-input"


nextButtonId : String
nextButtonId =
    "next-button"


playButtonId : String
playButtonId =
    "play-button"



-- SUBSCRIPTIONS
-- Keyboard shortcuts for self-evaluation in speak mode; Enter elsewhere
-- works via focused buttons, no subscription needed.


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.screen of
        InSession s ->
            case s.phase of
                SpeakEvaluating ->
                    Browser.Events.onKeyDown
                        (Decode.field "key" Decode.string
                            |> Decode.andThen
                                (\key ->
                                    case key of
                                        "1" ->
                                            Decode.succeed (SelfEvaluated True)

                                        "2" ->
                                            Decode.succeed (SelfEvaluated False)

                                        _ ->
                                            Decode.fail "unused key"
                                )
                        )

                _ ->
                    Sub.none

        _ ->
            Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ A.class "app" ]
        (case model.screen of
            Setup ->
                viewSetup model.form

            InSession s ->
                viewSession s

            Finished s ->
                viewResults s
        )


viewSetup : SetupForm -> List (Html Msg)
viewSetup form =
    [ h1 [] [ text "Portuguese number drill" ]
    , section []
        [ h2 [] [ text "Mode" ]
        , choiceButton (form.mode == Listen) (ModeChosen Listen) "Listen — hear a number, type it"
        , choiceButton (form.mode == Speak) (ModeChosen Speak) "Speak — see a number, say it aloud"
        ]
    , section []
        [ h2 [] [ text "Number range" ]
        , div [] (List.map presetButton [ ( 0, 9 ), ( 0, 99 ), ( 0, 999 ), ( 0, 9999 ) ])
        , div []
            [ label [] [ text "from ", numberInput form.minInput MinChanged ]
            , label [] [ text " to ", numberInput form.maxInput MaxChanged ]
            ]
        ]
    , section []
        [ h2 [] [ text "Session goal" ]
        , choiceButton (form.goalKind == ReachCorrect)
            (GoalKindChosen ReachCorrect)
            "Reach N correct answers"
        , choiceButton (form.goalKind == FixedAttempts)
            (GoalKindChosen FixedAttempts)
            "Do N attempts"
        , div [] [ label [] [ text (goalPrompt form.goalKind ++ " "), numberInput form.goalInput GoalChanged ] ]
        ]
    , case form.error of
        Just err ->
            p [ A.class "error" ] [ text err ]

        Nothing ->
            text ""
    , button [ E.onClick StartClicked ] [ text "Start" ]
    ]


goalPrompt : GoalKind -> String
goalPrompt kind =
    case kind of
        ReachCorrect ->
            "Correct answers to reach:"

        FixedAttempts ->
            "Number of attempts:"


choiceButton : Bool -> Msg -> String -> Html Msg
choiceButton isSelected msg lbl =
    button [ E.onClick msg, A.classList [ ( "selected", isSelected ) ] ] [ text lbl ]


presetButton : ( Int, Int ) -> Html Msg
presetButton ( lo, hi ) =
    button [ E.onClick (PresetChosen lo hi) ]
        [ text (String.fromInt lo ++ "–" ++ String.fromInt hi) ]


numberInput : String -> (String -> Msg) -> Html Msg
numberInput val toMsg =
    input [ A.type_ "number", A.value val, E.onInput toMsg ] []


viewSession : Session -> List (Html Msg)
viewSession s =
    p [ A.class "progress" ] [ text (progressText s) ]
        :: viewPhase s
        ++ [ button [ A.class "quit", E.onClick ChangeSettingsClicked ] [ text "✕ Exit session" ] ]


progressText : Session -> String
progressText s =
    let
        prefix =
            modeName s.mode
                ++ " · "
                ++ String.fromInt s.rangeMin
                ++ "–"
                ++ String.fromInt s.rangeMax
                ++ " · "
    in
    case s.goalKind of
        ReachCorrect ->
            prefix
                ++ ("correct " ++ String.fromInt s.correct ++ " / " ++ String.fromInt s.goalTarget)
                ++ (" · mistakes " ++ String.fromInt s.wrong)

        FixedAttempts ->
            prefix
                ++ ("attempts " ++ String.fromInt (s.correct + s.wrong) ++ " / " ++ String.fromInt s.goalTarget)
                ++ (" · correct " ++ String.fromInt s.correct)


modeName : Mode -> String
modeName mode =
    case mode of
        Listen ->
            "Listen"

        Speak ->
            "Speak"


viewPhase : Session -> List (Html Msg)
viewPhase s =
    case s.phase of
        Loading ->
            []

        ListenAnswering answer ->
            [ p [] [ text "Type the number you hear:" ]
            , div []
                [ input
                    [ A.id answerInputId
                    , A.type_ "text"
                    , A.attribute "inputmode" "numeric"
                    , A.autocomplete False
                    , A.value answer
                    , E.onInput AnswerChanged
                    , onEnter AnswerSubmitted
                    ]
                    []
                , button [ E.onClick AnswerSubmitted ] [ text "Check" ]
                ]
            , replayRow
            ]

        ListenVerdict { given, wasCorrect } ->
            [ p []
                (if wasCorrect then
                    [ span [ A.class "ok" ] [ text "✓ Correct!" ] ]

                 else
                    [ span [ A.class "bad" ] [ text "✗ Wrong" ]
                    , span [ A.class "note" ] [ text (" — correct was " ++ String.fromInt s.current) ]
                    ]
                )
            , div []
                [ input [ A.type_ "text", A.value given, A.readonly True ] []
                , button [ A.id nextButtonId, E.onClick NextClicked ] [ text "Next ⏎" ]
                ]
            , replayRow
            ]

        SpeakThinking ->
            [ p [] [ text "Say this number out loud in Portuguese:" ]
            , p [ A.class "big-number" ] [ text (String.fromInt s.current) ]
            , div [] [ button [ A.id playButtonId, E.onClick RevealClicked ] [ text "🔊 Hear it ⏎" ] ]
            ]

        SpeakEvaluating ->
            [ p [] [ text "How did it go?" ]
            , p [ A.class "big-number" ] [ text (String.fromInt s.current) ]
            , div []
                [ button [ E.onClick (SelfEvaluated True) ] [ text "✓ I said it right (1)" ]
                , button [ E.onClick (SelfEvaluated False) ] [ text "✗ I made a mistake (2)" ]
                ]
            , replayRow
            ]


replayRow : Html Msg
replayRow =
    div [] [ button [ E.onClick ReplayClicked ] [ text "🔊 Play again" ] ]


viewResults : Session -> List (Html Msg)
viewResults s =
    [ h1 [] [ text "Session complete" ]
    , p [] [ text (summaryText s) ]
    , p [] [ text (accuracyText s) ]
    , p [] [ text (averageTimeText s) ]
    , div []
        [ button [ E.onClick RestartClicked ] [ text "Practice again" ]
        , button [ E.onClick ChangeSettingsClicked ] [ text "Change settings" ]
        ]
    ]


summaryText : Session -> String
summaryText s =
    case s.goalKind of
        ReachCorrect ->
            "You reached "
                ++ String.fromInt s.goalTarget
                ++ " correct answers with "
                ++ String.fromInt s.wrong
                ++ " mistakes along the way."

        FixedAttempts ->
            "You got "
                ++ String.fromInt s.correct
                ++ " out of "
                ++ String.fromInt (s.correct + s.wrong)
                ++ " correct."


accuracyText : Session -> String
accuracyText s =
    let
        total =
            s.correct + s.wrong
    in
    if total == 0 then
        ""

    else
        "Accuracy: " ++ String.fromInt (round (100 * toFloat s.correct / toFloat total)) ++ "%"


averageTimeText : Session -> String
averageTimeText s =
    case s.answerMillis of
        [] ->
            ""

        times ->
            let
                tenths =
                    round (toFloat (List.sum times) / toFloat (List.length times) / 100)
            in
            "Average time per answer: "
                ++ String.fromInt (tenths // 10)
                ++ "."
                ++ String.fromInt (remainderBy 10 tenths)
                ++ " s"


onEnter : Msg -> Html.Attribute Msg
onEnter msg =
    E.on "keydown"
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    if key == "Enter" then
                        Decode.succeed msg

                    else
                        Decode.fail "not enter"
                )
        )
