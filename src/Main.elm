module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import List
import Random


main : Program Int Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


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


type alias Model =
    { seed : Random.Seed
    , stations : List StationState
    , score : Int
    , jetsOn : Bool
    , endTimer : Countdown
    }


emptyStationGenerator : Random.Generator StationState
emptyStationGenerator =
    Random.map2
        Empty
        (Random.uniform 1000 [ 1200, 1400, 1600, 1800, 2000 ])
        (Random.uniform Rare [ Medium, WellDone ])


init : Int -> ( Model, Cmd Msg )
init randomInt =
    let
        initialSeed : Random.Seed
        initialSeed =
            Random.initialSeed randomInt

        ( station1, seed1 ) =
            Random.step emptyStationGenerator initialSeed

        ( station2, seed2 ) =
            Random.step emptyStationGenerator seed1

        ( station3, seed3 ) =
            Random.step emptyStationGenerator seed2

        initialStations : List StationState
        initialStations =
            [ station1
            , station2
            , station3
            ]
    in
    ( { seed = seed3
      , stations = initialStations
      , score = 0
      , jetsOn = False
      , endTimer = 1000 * 30
      }
    , Cmd.none
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
    case doneness of
        Rare ->
            if progress >= 0.5 && progress < 0.6 then
                1

            else
                0

        Medium ->
            if progress >= 0.6 && progress < 0.8 then
                1

            else
                0

        WellDone ->
            if progress >= 0.8 && progress < 0.95 then
                1

            else
                0


burgerCookTimeMs : Float
burgerCookTimeMs =
    5000


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetSeed newSeed ->
            ( { model | seed = newSeed }, Cmd.none )

        HandleAnimationFrameDelta delta ->
            let
                updateStation : StationState -> StationState
                updateStation stationState =
                    case stationState of
                        Cooking doneness progress ->
                            let
                                progressDelta : Progress
                                progressDelta =
                                    delta / burgerCookTimeMs

                                adjustedProgressDelta : Progress
                                adjustedProgressDelta =
                                    if model.jetsOn then
                                        progressDelta * 2

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
            in
            ( { model
                | stations = List.map updateStation model.stations
                , endTimer = model.endTimer - delta
              }
            , Cmd.none
            )

        StartStation index ->
            let
                updateStation : StationState -> StationState
                updateStation stationState =
                    case stationState of
                        ReadyToStart doneness ->
                            Cooking doneness 0

                        _ ->
                            stationState
            in
            ( { model
                | stations = updateIndex updateStation index model.stations
              }
            , Cmd.none
            )

        ServeStation index ->
            case getIndex index model.stations of
                Just (Cooking doneness progress) ->
                    let
                        ( emptyStation, newSeed ) =
                            Random.step emptyStationGenerator model.seed
                    in
                    ( { model
                        | stations = updateIndex (\_ -> emptyStation) index model.stations
                        , score = model.score + getScore doneness progress
                        , seed = newSeed
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        TossStation index ->
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
                | stations = updateIndex updateStation index model.stations
                , seed = newSeed
              }
            , Cmd.none
            )

        HandleJetsOff ->
            ( { model | jetsOn = False }, Cmd.none )

        HandleJetsOn ->
            ( { model | jetsOn = True }, Cmd.none )



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
    case maybeShortcut of
        Nothing ->
            NoOp

        Just (ShortcutStation index) ->
            case getIndex index model.stations of
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


keyDownShortcutToMsg : Model -> Maybe Shortcut -> Msg
keyDownShortcutToMsg model maybeShortcut =
    case maybeShortcut of
        Just ShortcutJets ->
            HandleJetsOn

        _ ->
            NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
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
    if model.endTimer <= 0 then
        Sub.none

    else
        Sub.batch
            [ Browser.Events.onAnimationFrameDelta HandleAnimationFrameDelta
            , Browser.Events.onKeyUp keyUpDecoder
            , Browser.Events.onKeyDown keyDownDecoder
            ]



-- VIEW


columnClass : Html.Attribute Msg
columnClass =
    class "flex flex-col items-center gap-2"


buttonClass : Html.Attribute Msg
buttonClass =
    class "py-1 px-2 bg-gray-200 border border-black rounded-sm"


renderStation : Bool -> Int -> StationState -> Html Msg
renderStation jetsOn index stationState =
    let
        stationNumber : Int
        stationNumber =
            index + 1
    in
    div [ class "flex items-center w-full border border-black overflow-hidden h-16" ]
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


renderStationContent : Bool -> StationState -> Html Msg
renderStationContent jetsOn stationState =
    case stationState of
        Empty _ _ ->
            div [ class "p-2 italic" ] [ text "Empty" ]

        ReadyToStart doneness ->
            div [ class "p-2" ] [ text <| "Order: " ++ donenessLabel doneness ]

        Cooking doneness progress ->
            div [ class "w-full h-full" ] [ renderCookingState jetsOn doneness progress ]

        Burnt ->
            div [ class "p-2 font-bold" ] [ text "Burnt!" ]


renderCookingState : Bool -> Doneness -> Progress -> Html Msg
renderCookingState jetsOn doneness progress =
    let
        ( donenessLeftStyle, donenessWidthStyle, donenessBgClass ) =
            case doneness of
                Rare ->
                    ( "50%", "10%", class "bg-red-200" )

                Medium ->
                    ( "60%", "20%", class "bg-red-700" )

                WellDone ->
                    ( "80%", "15%", class "bg-red-900" )

        progressStyleLeft =
            String.fromFloat (progress * 100) ++ "%"

        progressBorderClass =
            if jetsOn then
                class "border-yellow-400"

            else
                class "border-blue-500"
    in
    div [ class "w-full h-full relative" ]
        -- renderDoneness
        [ div
            [ class "h-full absolute text-gray-100 flex items-center justify-center"
            , donenessBgClass
            , style "left" donenessLeftStyle
            , style "width" donenessWidthStyle
            ]
            [ text (donenessLabel doneness) ]

        -- Render progress
        , div [ class "absolute h-full border z-1", progressBorderClass, style "left" progressStyleLeft ] []
        ]


renderStationButton : Int -> StationState -> Html Msg
renderStationButton index stationState =
    case stationState of
        Empty _ _ ->
            div [] []

        ReadyToStart doneness ->
            button [ buttonClass, onClick <| StartStation index ] [ text "Start" ]

        Cooking doneness progress ->
            button [ buttonClass, onClick <| ServeStation index ] [ text "Serve" ]

        Burnt ->
            button [ buttonClass, onClick <| TossStation index ] [ text "Toss" ]


renderShortcut : String -> Html Msg
renderShortcut shortcut =
    span [ class "font-bold" ] [ text <| "[" ++ shortcut ++ "]" ]


view : Model -> Html Msg
view model =
    div [ columnClass, class "w-full p-10" ]
        [ div [ class "flex items-center w-full" ] [ span [ class "text-2xl" ] [ text "Burger Boss" ] ]
        , div [ class "flex gap-1 w-full" ]
            [ div [ columnClass, class "w-1/2 relative" ]
                [ div [ columnClass, class "w-full" ]
                    (List.indexedMap (renderStation model.jetsOn) model.stations)
                , button [ onMouseDown HandleJetsOn, onMouseUp HandleJetsOff, buttonClass ] [ text "! Jets !" ]
                , div [ class "absolute bg-opacity-50 bg-black w-full h-full flex justify-center items-center", classList [ ( "hidden", model.endTimer > 0 ) ] ]
                    [ span [ class "text-8xl font-bold text-red-500" ] [ text "Shop Closed" ] ]
                ]
            , div [ columnClass, class "flex-grow" ]
                [ div [ class "flex items-center gap-1" ]
                    [ div [] [ text "Day timer:" ]
                    , div [] [ text (String.fromInt (Basics.max 0 (floor (model.endTimer / 1000)))) ]
                    ]
                , div [ class "flex items-center gap-1" ]
                    [ span [] [ text "Burgers served:" ]
                    , span [] [ text (String.fromInt model.score) ]
                    ]
                , div [ class "flex items-center gap-1 text-sm" ]
                    [ span [] [ text "(Par:" ]
                    , span [] [ text "12)" ]
                    ]
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
                            [ text ": Start, Serve, or Toss burgers"
                            ]
                        ]
                    , div []
                        [ renderShortcut "Space"
                        , span []
                            [ text ": Toggle Jets"
                            ]
                        ]
                    ]
                ]
            ]
        ]
