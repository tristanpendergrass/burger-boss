port module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra.Pointer as Pointer
import Json.Decode as D
import Json.Encode as E
import List
import Random


port saveScores : E.Value -> Cmd msg


type alias Flags =
    { initialSeed : Int, highScores : List Int }


main : Program Flags Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias ZoneCoords =
    -- (Start %, End %)
    ( Int, Int )


getZoneCoords : Doneness -> ZoneCoords
getZoneCoords doneness =
    case doneness of
        Rare ->
            ( 50, 60 )

        Medium ->
            ( 60, 80 )

        WellDone ->
            ( 80, 95 )


type Doneness
    = Rare
    | Medium
    | WellDone


type alias Progress =
    Float


type alias Countdown =
    -- The number of milliseconds before something will happen
    Float


type StationState
    = Empty Countdown Doneness -- The doneness is what the order will eventually be
    | ReadyToStart Doneness
    | Cooking Doneness Progress
    | Burnt


type alias GameState =
    { score : Int
    , jetsOn : Bool
    , endTimer : Countdown
    , stations : List StationState
    }


type State
    = MainMenu
    | InGame GameState
    | GameOver Int


type alias Model =
    { seed : Random.Seed
    , state : State
    , showHowToPlay : Bool
    , highScores : List Int
    }


emptyStationGenerator : Random.Generator StationState
emptyStationGenerator =
    Random.map2
        Empty
        (Random.uniform 1000 [ 1200, 1400, 1600, 1800, 2000 ])
        (Random.uniform Rare [ Medium, WellDone ])


init : Flags -> ( Model, Cmd Msg )
init { initialSeed, highScores } =
    ( { seed = Random.initialSeed initialSeed
      , state = MainMenu
      , showHowToPlay = False
      , highScores = highScores
      }
    , Cmd.none
    )


gameDuration : Float
gameDuration =
    300 * 1000


gameStateGenerator : Random.Generator GameState
gameStateGenerator =
    Random.list 5 emptyStationGenerator
        |> Random.map
            (\stations ->
                { score = 0
                , jetsOn = False
                , endTimer = gameDuration
                , stations = stations
                }
            )



-- UPDATE


type Msg
    = NoOp
    | SetSeed Random.Seed
    | HandleAnimationFrameDelta Float
    | StartStation Int
    | ServeStation Int
    | TossStation Int
    | HandleJetsOn
    | HandleJetsOff
    | HandleStartGameClick
    | HandleMainMenuClick
    | HandleStationClick Int
    | HandleHowToPlayClick
    | HandleCloseHowToPlay
    | HandleCloseEarlyClick


updateIndex : (a -> a) -> Int -> List a -> List a
updateIndex fn index =
    List.indexedMap
        (\i v ->
            if i == index then
                fn v

            else
                v
        )


getIndex : Int -> List a -> Maybe a
getIndex index =
    List.drop index >> List.head


getScore : Doneness -> Progress -> Int
getScore doneness progress =
    let
        progressPercent : Int
        progressPercent =
            floor (progress * 100)

        ( startPercent, endPercent ) =
            getZoneCoords doneness
    in
    if
        progressPercent
            >= (startPercent
                    -- We add this adjustment to account for the width of the burger image which is set at 5%
                    - 8
               )
            && progressPercent
            < endPercent
    then
        1

    else
        0


burgerCookTimeMs : Float
burgerCookTimeMs =
    12000


handleStationDelta : Bool -> Float -> StationState -> StationState
handleStationDelta jetsOn delta stationState =
    case stationState of
        Cooking doneness progress ->
            let
                progressDelta : Progress
                progressDelta =
                    delta / burgerCookTimeMs

                adjustedProgressDelta : Progress
                adjustedProgressDelta =
                    if jetsOn then
                        progressDelta * 6

                    else
                        progressDelta

                newProgress : Progress
                newProgress =
                    progress + adjustedProgressDelta
            in
            if newProgress > 1 then
                Burnt

            else
                Cooking doneness newProgress

        Empty countdown doneness ->
            let
                newCountdown : Countdown
                newCountdown =
                    countdown - delta
            in
            if newCountdown <= 0 then
                ReadyToStart doneness

            else
                Empty newCountdown doneness

        _ ->
            stationState


updateHighScores : Int -> List Int -> ( List Int, Cmd Msg )
updateHighScores score highScores =
    let
        newHighScores : List Int
        newHighScores =
            score :: highScores
    in
    ( newHighScores, saveHighScores newHighScores )


saveHighScores : List Int -> Cmd msg
saveHighScores scores =
    saveScores <| E.list E.int scores


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noOp =
            ( model, Cmd.none )
    in
    case msg of
        NoOp ->
            noOp

        SetSeed newSeed ->
            ( { model | seed = newSeed }, Cmd.none )

        HandleAnimationFrameDelta delta ->
            case model.state of
                InGame gameState ->
                    let
                        newEndTimer : Countdown
                        newEndTimer =
                            gameState.endTimer - delta
                    in
                    if newEndTimer <= 0 then
                        let
                            ( newHighScores, updateHighScoresCmd ) =
                                updateHighScores gameState.score model.highScores
                        in
                        ( { model | state = GameOver gameState.score, highScores = newHighScores }, updateHighScoresCmd )

                    else
                        ( { model
                            | state =
                                InGame
                                    { gameState
                                        | stations = List.map (handleStationDelta gameState.jetsOn delta) gameState.stations
                                        , endTimer = gameState.endTimer - delta
                                    }
                          }
                        , Cmd.none
                        )

                _ ->
                    noOp

        HandleStationClick index ->
            case model.state of
                InGame gameState ->
                    case getIndex index gameState.stations of
                        Just (ReadyToStart doneness) ->
                            let
                                updateStation : StationState -> StationState
                                updateStation _ =
                                    Cooking doneness 0
                            in
                            ( { model | state = InGame { gameState | stations = updateIndex updateStation index gameState.stations } }, Cmd.none )

                        Just (Cooking doneness progress) ->
                            let
                                ( emptyStation, newSeed ) =
                                    Random.step emptyStationGenerator model.seed
                            in
                            ( { model
                                | state =
                                    InGame
                                        { gameState
                                            | stations = updateIndex (\_ -> emptyStation) index gameState.stations
                                            , score = gameState.score + getScore doneness progress
                                        }
                                , seed = newSeed
                              }
                            , Cmd.none
                            )

                        Just Burnt ->
                            let
                                ( emptyStation, newSeed ) =
                                    Random.step emptyStationGenerator model.seed

                                updateStation : StationState -> StationState
                                updateStation _ =
                                    emptyStation
                            in
                            ( { model
                                | state =
                                    InGame
                                        { gameState
                                            | stations = updateIndex updateStation index gameState.stations
                                        }
                                , seed = newSeed
                              }
                            , Cmd.none
                            )

                        _ ->
                            noOp

                _ ->
                    noOp

        StartStation index ->
            case model.state of
                InGame gameState ->
                    let
                        updateStation : StationState -> StationState
                        updateStation stationState =
                            case stationState of
                                ReadyToStart doneness ->
                                    Cooking doneness 0

                                _ ->
                                    stationState
                    in
                    ( { model | state = InGame { gameState | stations = updateIndex updateStation index gameState.stations } }, Cmd.none )

                _ ->
                    noOp

        ServeStation index ->
            case model.state of
                InGame gameState ->
                    case getIndex index gameState.stations of
                        Just (Cooking doneness progress) ->
                            let
                                ( emptyStation, newSeed ) =
                                    Random.step emptyStationGenerator model.seed
                            in
                            ( { model
                                | state =
                                    InGame
                                        { gameState
                                            | stations = updateIndex (\_ -> emptyStation) index gameState.stations
                                            , score = gameState.score + getScore doneness progress
                                        }
                                , seed = newSeed
                              }
                            , Cmd.none
                            )

                        _ ->
                            noOp

                _ ->
                    noOp

        TossStation index ->
            case model.state of
                InGame gameState ->
                    let
                        ( emptyStation, newSeed ) =
                            Random.step emptyStationGenerator model.seed

                        updateStation : StationState -> StationState
                        updateStation stationState =
                            case stationState of
                                Burnt ->
                                    emptyStation

                                _ ->
                                    stationState
                    in
                    ( { model
                        | state =
                            InGame
                                { gameState
                                    | stations = updateIndex updateStation index gameState.stations
                                }
                        , seed = newSeed
                      }
                    , Cmd.none
                    )

                _ ->
                    noOp

        HandleJetsOff ->
            case model.state of
                InGame gameState ->
                    ( { model | state = InGame { gameState | jetsOn = False } }, Cmd.none )

                _ ->
                    noOp

        HandleJetsOn ->
            case model.state of
                InGame gameState ->
                    ( { model | state = InGame { gameState | jetsOn = True } }, Cmd.none )

                _ ->
                    noOp

        HandleStartGameClick ->
            case model.state of
                MainMenu ->
                    let
                        ( gameState, newSeed ) =
                            Random.step gameStateGenerator model.seed
                    in
                    ( { model | seed = newSeed, state = InGame gameState }, Cmd.none )

                _ ->
                    noOp

        HandleMainMenuClick ->
            ( { model | state = MainMenu }, Cmd.none )

        HandleHowToPlayClick ->
            ( { model | showHowToPlay = True }, Cmd.none )

        HandleCloseHowToPlay ->
            ( { model | showHowToPlay = False }, Cmd.none )

        HandleCloseEarlyClick ->
            case model.state of
                InGame gameState ->
                    let
                        ( newHighScores, updateHighScoresCmd ) =
                            updateHighScores gameState.score model.highScores
                    in
                    ( { model | state = GameOver gameState.score, highScores = newHighScores }, updateHighScoresCmd )

                _ ->
                    noOp



-- SUBSCRIPTIONS


type Shortcut
    = ShortcutStation Int
    | ShortcutJets


stringToShortcut : String -> Maybe Shortcut
stringToShortcut str =
    case str of
        "1" ->
            Just (ShortcutStation 0)

        "2" ->
            Just (ShortcutStation 1)

        "3" ->
            Just (ShortcutStation 2)

        "4" ->
            Just (ShortcutStation 3)

        "5" ->
            Just (ShortcutStation 4)

        " " ->
            Just ShortcutJets

        _ ->
            Nothing


keyUpShortcutToMsg : Model -> Maybe Shortcut -> Msg
keyUpShortcutToMsg model maybeShortcut =
    case model.state of
        InGame gameState ->
            case maybeShortcut of
                Nothing ->
                    NoOp

                Just (ShortcutStation index) ->
                    case getIndex index gameState.stations of
                        Nothing ->
                            NoOp

                        Just stationState ->
                            case stationState of
                                Empty _ _ ->
                                    NoOp

                                ReadyToStart _ ->
                                    StartStation index

                                Cooking _ _ ->
                                    ServeStation index

                                Burnt ->
                                    TossStation index

                Just ShortcutJets ->
                    HandleJetsOff

        _ ->
            NoOp


keyDownShortcutToMsg : Model -> Maybe Shortcut -> Msg
keyDownShortcutToMsg model maybeShortcut =
    case maybeShortcut of
        Just ShortcutJets ->
            HandleJetsOn

        _ ->
            NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        InGame gameState ->
            let
                keyUpDecoder : D.Decoder Msg
                keyUpDecoder =
                    D.field "key" D.string
                        |> D.map stringToShortcut
                        |> D.map (keyUpShortcutToMsg model)

                keyDownDecoder : D.Decoder Msg
                keyDownDecoder =
                    D.field "key" D.string
                        |> D.map stringToShortcut
                        |> D.map (keyDownShortcutToMsg model)
            in
            if gameState.endTimer <= 0 then
                Sub.none

            else
                Sub.batch
                    [ Browser.Events.onAnimationFrameDelta HandleAnimationFrameDelta
                    , Browser.Events.onKeyUp keyUpDecoder
                    , Browser.Events.onKeyDown keyDownDecoder
                    ]

        _ ->
            Sub.none



-- VIEW


columnClass : Html.Attribute Msg
columnClass =
    class "flex flex-col items-center gap-2"


buttonClass : Html.Attribute Msg
buttonClass =
    class "py-1 px-2 bg-gray-100 hover:bg-gray-200 active:bg-gray-300 border border-black rounded-sm text-sm"


renderStation : Bool -> Int -> StationState -> Html Msg
renderStation jetsOn index stationState =
    let
        stationNumber : Int
        stationNumber =
            index + 1
    in
    div [ class "flex items-center w-full border border-black overflow-hidden h-16 cursor-pointer", Pointer.onDown (\_ -> HandleStationClick index) ]
        [ div [ class "w-28 border-r border-black h-full flex items-center justify-center" ]
            [ div [] [ text ("Station " ++ String.fromInt stationNumber) ]
            ]
        , div [ class "w-full h-full border-r border-black flex items-center" ] [ renderStationContent jetsOn stationState ]
        , div [ class "w-28 h-full flex items-center justify-center" ]
            [ renderStationButton index stationState
            ]
        ]


donenessLabel : Doneness -> String
donenessLabel doneness =
    case doneness of
        Rare ->
            "Rare"

        Medium ->
            "Medium"

        WellDone ->
            "Well Done"


stationBgClass : Attribute Msg
stationBgClass =
    class "bg-gray-200"


renderStationContent : Bool -> StationState -> Html Msg
renderStationContent jetsOn stationState =
    case stationState of
        Empty _ _ ->
            div [ class "p-2 italic" ] []

        ReadyToStart doneness ->
            div [ class "p-2 w-full h-full flex items-center gap-1 bg-green-100" ] [ span [] [ text "Order up:" ], span [ class "italic" ] [ text (donenessLabel doneness) ] ]

        Cooking doneness progress ->
            div [ class "w-full h-full" ] [ renderCookingState jetsOn doneness progress ]

        Burnt ->
            div [ class "p-2 font-bold w-full h-full flex items-center gap-1 bg-red-100" ] [ text "Burnt!" ]


renderCookingState : Bool -> Doneness -> Progress -> Html Msg
renderCookingState jetsOn doneness progress =
    let
        donenessBgClass : Attribute Msg
        donenessBgClass =
            case doneness of
                Rare ->
                    class "bg-red-200"

                Medium ->
                    class "bg-red-700"

                WellDone ->
                    class "bg-red-900"

        ( zoneCoordsStart, zoneCoordsEnd ) =
            getZoneCoords doneness

        ( donenessLeftStyle, donenessWidthStyle ) =
            ( String.fromInt zoneCoordsStart ++ "%", String.fromInt (zoneCoordsEnd - zoneCoordsStart) ++ "%" )

        progressStyleLeft =
            String.fromFloat (progress * 100) ++ "%"
    in
    div [ class "w-full h-full relative overflow-hidden", stationBgClass ]
        -- renderDoneness
        [ div
            [ class "h-full absolute text-gray-100 flex items-center justify-center text-sm"
            , donenessBgClass
            , style "left" donenessLeftStyle
            , style "width" donenessWidthStyle
            ]
            [ text (donenessLabel doneness) ]

        -- Render progress
        , img
            [ class "absolute z-1 w-[8%]"
            , style "left" progressStyleLeft
            , style "top" "50%"
            , style "transform" "translateY(-50%)"
            , src "Patty3.png"
            ]
            []
        ]


onDownStopPropagation =
    Pointer.onWithOptions "pointerdown" { stopPropagation = True, preventDefault = False }


renderStationButton : Int -> StationState -> Html Msg
renderStationButton index stationState =
    let
        shortcutText : Html Msg
        shortcutText =
            renderShortcut (String.fromInt (index + 1))
    in
    case stationState of
        Empty _ _ ->
            div [] []

        ReadyToStart doneness ->
            button [ buttonClass, onDownStopPropagation (\_ -> StartStation index) ] [ text "Start ", shortcutText ]

        Cooking doneness progress ->
            button [ buttonClass, onDownStopPropagation (\_ -> ServeStation index) ] [ text "Serve ", shortcutText ]

        Burnt ->
            button [ buttonClass, onDownStopPropagation (\_ -> TossStation index) ] [ text "Toss ", shortcutText ]


renderShortcut : String -> Html Msg
renderShortcut shortcut =
    strong [] [ text ("[" ++ shortcut ++ "]") ]


countdownToBool : Countdown -> Bool
countdownToBool countdown =
    countdown
        / 100
        -- convert to quarter seconds
        |> floor
        |> modBy 2
        |> (==) 0


flames1 : GameState -> Html Msg
flames1 gameState =
    let
        jetsClass : Attribute Msg
        jetsClass =
            class "w-auto h-full"
    in
    div [ class "h-full", classList [ ( "hidden", not gameState.jetsOn ) ], class "bg-gradient-to-t from-yellow-300 to-transparent" ]
        [ img [ src "Jets11.png", jetsClass, classList [ ( "hidden", countdownToBool gameState.endTimer ) ] ] []
        , img [ src "Jets12.png", jetsClass, classList [ ( "hidden", not (countdownToBool gameState.endTimer) ) ] ] []
        ]


flames2 : GameState -> Html Msg
flames2 gameState =
    let
        jetsClass : Attribute Msg
        jetsClass =
            class "w-auto h-full"
    in
    div [ class "h-full", classList [ ( "hidden", not gameState.jetsOn ) ], class "bg-gradient-to-t from-yellow-300 to-transparent" ]
        [ img [ src "Jets11.png", jetsClass, classList [ ( "hidden", not (countdownToBool gameState.endTimer) ) ] ] []
        , img [ src "Jets12.png", jetsClass, classList [ ( "hidden", countdownToBool gameState.endTimer ) ] ] []
        ]


renderHowToPlay : Model -> Html Msg
renderHowToPlay model =
    div
        [ classList [ ( "hidden", not model.showHowToPlay ) ]
        , class "fixed top-0 left-0 bg-white bg-opacity-50 w-screen h-screen flex items-start justify-center p-16"
        , Pointer.onDown (\_ -> HandleCloseHowToPlay)
        ]
        [ div
            [ columnClass
            , class "prose bg-white border border-gray-900 rounded max-h-[90%] overflow-auto p-16 shadow-xl relative max-w-[90%]"
            , Pointer.onWithOptions "pointerdown" { stopPropagation = True, preventDefault = False } (\_ -> NoOp)
            ]
            [ h1 [] [ text "Credits" ]
            , p [ class "italic" ] [ text "This game is dedicated to my Mom. Happy Birthday!" ]
            , p [] [ text "Game design and development: Tristan" ]
            , p [] [ text "Supplemental art: Yang" ]
            , div [ class "bg-sky-200 rounded-xl relative" ]
                [ img [ src "pup_transparent.png", alt "Confused Dog", width 400 ] []
                ]
            , button [ buttonClass, class "mt-8", onDownStopPropagation (\_ -> HandleCloseHowToPlay) ] [ text "Close" ]
            ]
        ]


renderHighScore : ( String, Int ) -> Html Msg
renderHighScore ( name, score ) =
    tr [] [ td [ class "uppercase font-semibold" ] [ text name ], td [] [ text (String.fromInt score) ] ]


renderBirthday : Html Msg
renderBirthday =
    span [ class "flex items-center gap-2 text-3xl font-bold bg-sky-200 px-2 py-1 rounded" ]
        [ span [ class "text-red-500" ] [ text "B" ]
        , span [ class "text-blue-500" ] [ text "i" ]
        , span [ class "text-green-500" ] [ text "r" ]
        , span [ class "text-yellow-500" ] [ text "t" ]
        , span [ class "text-purple-500" ] [ text "h" ]
        , span [ class "text-red-500" ] [ text "d" ]
        , span [ class "text-blue-500" ] [ text "a" ]
        , span [ class "text-green-500" ] [ text "y" ]
        , span [ class "text-yellow-500" ] [ text " " ]
        , span [ class "text-purple-500" ] [ text "E" ]
        , span [ class "text-red-500" ] [ text "d" ]
        , span [ class "text-blue-500" ] [ text "i" ]
        , span [ class "text-green-500" ] [ text "t" ]
        , span [ class "text-yellow-500" ] [ text "i" ]
        , span [ class "text-purple-500" ] [ text "o" ]
        , span [ class "text-red-500" ] [ text "n" ]
        ]


view : Model -> Html Msg
view model =
    div [ columnClass, class "w-full p-10" ]
        [ div [ class "w-full" ]
            [ div [ class "flex items-center justify-between w-1/2" ]
                [ span [ class "text-3xl font-bold" ] [ text "Burger Boss" ]
                , renderBirthday
                ]
            ]
        , div [ class "flex gap-1 w-full" ]
            [ let
                leftSideClass : Attribute Msg
                leftSideClass =
                    class "w-1/2 relative min-h-96 border border-gray-900 rounded-2xl p-4 transition-all"
              in
              case model.state of
                InGame gameState ->
                    div [ columnClass, leftSideClass, class "bg-gray-100" ]
                        [ div [ class "w-full flex items-center justify-center" ]
                            [ div [ class "prose" ]
                                [ text "Burgers "
                                , text "served: "
                                , strong [] [ text (String.fromInt gameState.score) ]
                                ]
                            ]
                        , div [ columnClass, class "w-full" ]
                            (List.indexedMap (renderStation gameState.jetsOn) gameState.stations)
                        , div [ class "w-full flex gap-12 items-center justify-center h-16" ]
                            [ flames1 gameState
                            , button
                                [ Pointer.onWithOptions "pointerdown" { stopPropagation = False, preventDefault = True } (\_ -> HandleJetsOn)
                                , Pointer.onWithOptions "pointerup" { stopPropagation = False, preventDefault = True } (\_ -> HandleJetsOff)
                                , Pointer.onOut (\_ -> HandleJetsOff)
                                , buttonClass
                                , class "bg-yellow-500 hover:bg-yellow-400 active:bg-yellow-300"
                                , classList [ ( "bg-yellow-500", not gameState.jetsOn ), ( "bg-yellow-300", gameState.jetsOn ) ]
                                ]
                                [ text "Activate Jets! ", renderShortcut "Space" ]
                            , flames2 gameState
                            ]
                        ]

                GameOver _ ->
                    div [ columnClass, leftSideClass ]
                        [ div [ class "w-full h-full bg-contain bg-no-repeat bg-center flex items-start justify-end bg-opacity-50", style "background-image" "url('storefront-closed.png')" ]
                            [ div [ class "text-red-500 text-6xl font-bold" ] [ text "Game Over" ] ]
                        ]

                MainMenu ->
                    div [ columnClass, leftSideClass ]
                        [ div [ class "w-full h-full bg-contain bg-no-repeat bg-center", style "background-image" "url('storefront-open.png')" ] [] ]
            , let
                rightSideClass : Attribute Msg
                rightSideClass =
                    class "flex-grow p-4"
              in
              case model.state of
                InGame gameState ->
                    div [ columnClass, rightSideClass ]
                        [ div [ class "flex items-center gap-1 text-xl border border-gray-900 px-4 py-2" ]
                            [ div [] [ text "Day timer:" ]
                            , div [] [ text (String.fromInt (Basics.max 0 (floor (gameState.endTimer / 1000)))) ]
                            ]
                        , button
                            [ buttonClass
                            , class "bg-red-300 hover:bg-red-200"
                            , Pointer.onDown (\_ -> HandleCloseEarlyClick)
                            ]
                            [ text "Close early" ]
                        , div [ columnClass, class "w-full border-t border-black mt-24" ]
                            [ div [] [ text "Keyboard Shortcuts" ]
                            , div []
                                [ renderShortcut "1"
                                , span []
                                    [ text ", "
                                    ]
                                , renderShortcut "2"
                                , span []
                                    [ text ", "
                                    ]
                                , renderShortcut "3"
                                , span []
                                    [ text ", "
                                    ]
                                , renderShortcut "4"
                                , span []
                                    [ text ", "
                                    ]
                                , renderShortcut "5"
                                , span []
                                    [ text ": Start, Serve, or Toss burgers"
                                    ]
                                ]
                            , div []
                                [ renderShortcut "Space"
                                , span []
                                    [ text ": Activate Jets!"
                                    ]
                                ]
                            ]
                        ]

                MainMenu ->
                    div [ columnClass, rightSideClass, class "gap-8" ]
                        [ button
                            [ buttonClass
                            , class "bg-green-300 hover:bg-green-200 active:bg-green-100"
                            , Pointer.onDown (\_ -> HandleStartGameClick)
                            ]
                            [ text "New Game" ]
                        , button
                            [ buttonClass
                            , Pointer.onDown (\_ -> HandleHowToPlayClick)
                            ]
                            [ text "Credits" ]
                        , renderHowToPlay model
                        , hr [ class "w-full text-gray-900" ] []
                        , div [ class "prose prose-sm md:prose-base" ]
                            [ h2 [] [ text "Top Burger Bosses" ]
                            , table []
                                (model.highScores
                                    |> List.map (\score -> ( "you", score ))
                                    |> List.append [ ( "Baffi", 2 ), ( "Bambi", 3 ), ( "Lv Bu", 10 ) ]
                                    |> List.sortBy Tuple.second
                                    |> List.reverse
                                    |> List.take 10
                                    |> List.map renderHighScore
                                )
                            ]
                        ]

                GameOver score ->
                    div [ columnClass, rightSideClass, class "gap-6" ]
                        [ h2 [ class "font-semibold text-2xl leading-none" ] [ text "Burgers served" ]
                        , h2 [ class "font-semibold text-2xl leading-none" ] [ text (String.fromInt score) ]
                        , button [ buttonClass, Pointer.onDown (\_ -> HandleMainMenuClick) ] [ text "Main Menu" ]
                        ]
            ]
        ]
