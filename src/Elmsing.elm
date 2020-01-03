port module Elmsing exposing (..)

import Html exposing (Html)
import Browser as Browser
import Html.Events as Events
import Html.Attributes as Attributes
import Array
import Array exposing (Array, get, indexedMap)
import Maybe
import Maybe exposing (andThen, withDefault)
import Random
import Random.Extra as RandomExtra
import Time
import String exposing (toInt, fromInt, fromFloat)
import Tuple exposing (first, second)

type Spin = Up | Down
type InitialConfiguration = Ups | Downs | Random Random.Seed
type alias SpinMatrix = Array (Array Spin)
type alias IsingModel =
  { matrix : SpinMatrix
  }

shape : SpinMatrix -> (Int, Int)
shape spinMatrix =
  let
    width = withDefault 0 <| Maybe.map Array.length <| get 0 spinMatrix
    height = Array.length spinMatrix
  in
    (height, width)

initMatrix : InitialConfiguration -> Int -> Int -> (SpinMatrix, Maybe Random.Seed)
initMatrix configuration height width =
  case configuration of
    Ups ->
      (Array.repeat height <| Array.repeat width Up, Nothing)
    Downs ->
      (Array.repeat height <| Array.repeat width Down, Nothing)
    Random seed ->
      let
        spinGenerator = Random.map (\b -> if b then Up else Down) RandomExtra.bool
        spinListGenerator = Random.list width spinGenerator
        spinMatrixGenerator = Random.list height spinListGenerator
        (spinMatrix,seedNext) = Random.step spinMatrixGenerator seed
      in
        (Array.map Array.fromList <| Array.fromList spinMatrix, Just seedNext)

flipSpinAt spinMatrix i j =
  let
    row = get i spinMatrix
    spin = row |> andThen get j
  in
    case spin of
      Just s ->
        let
          rowNew = Maybe.map (Array.set j (flipSpin s)) row
        in
          case rowNew of
            Just r -> Array.set i r spinMatrix
            _ -> spinMatrix
      _ -> spinMatrix

flipSpin spin =
  case spin of
    Up -> Down
    Down -> Up

boundaryCondition lowerLimit upperLimit index =
  if index < lowerLimit then
    upperLimit + index
  else if index > upperLimit - 1 then
    index - upperLimit
  else
    index

spinMagnetization spin =
  case spin of
    Up -> 1
    Down -> -1

spinMagnetizationAt : SpinMatrix -> Int -> Int -> Float
spinMagnetizationAt spinMatrix i j =
  let
    (height, width) = shape spinMatrix
    ii = boundaryCondition 0 height i
    jj = boundaryCondition 0 width j
  in
    (withDefault 0 <| Maybe.map spinMagnetization <| get ii spinMatrix) |> andThen get jj

totalMagnetization : SpinMatrix -> Float
totalMagnetization spinMatrix =
  let
    (height, width) = shape spinMatrix
    totalMagnetizationNext i j spinMatrixNext magnetization =
      if i == height - 1 && j == width - 1 then
        magnetization
      else
        let
          magnetizationNext = magnetization + spinMagnetizationAt spinMatrixNext i j
        in
          if j == width - 1 then
            totalMagnetizationNext (i + 1) 0 spinMatrixNext magnetizationNext
          else
            totalMagnetizationNext i (j + 1) spinMatrixNext magnetizationNext
  in
    totalMagnetizationNext 0 0 spinMatrix 0

pointEnergy : SpinMatrix -> Int -> Int -> Float -> Float -> Float
pointEnergy spinMatrix i j magneticFieldStrength interactionStrength =
  let
    up = spinMagnetizationAt spinMatrix (i - 1) j
    down = spinMagnetizationAt spinMatrix (i + 1) j
    left = spinMagnetizationAt spinMatrix i (j - 1)
    right = spinMagnetizationAt spinMatrix i (j + 1)
    spin = spinMagnetizationAt spinMatrix i j
    neighbourSum = up + down + left + right
  in
    -1 * magneticFieldStrength * spin - interactionStrength * spin * neighbourSum

totalEnergy spinMatrix magneticFieldStrength interactionStrength =
  let
    totalEnergyNext spinMatrixNext i j magneticFieldStrengthNext interactionStrengthNext energy =
      let
        (height, width) = shape spinMatrixNext
      in
        if i == height - 1 && j == width - 1 then
          energy
        else
          let
            energyNext = energy + pointEnergy spinMatrixNext i j magneticFieldStrengthNext (0.5 * interactionStrengthNext)
          in
            if j == width - 1 then
              totalEnergyNext spinMatrixNext (i + 1) 0 magneticFieldStrengthNext interactionStrengthNext energyNext
            else
              totalEnergyNext spinMatrixNext i (j + 1) magneticFieldStrengthNext interactionStrengthNext energyNext
  in
    totalEnergyNext spinMatrix 0 0 magneticFieldStrength interactionStrength 0

metropolis spinMatrix iterations seed magneticFieldStrength interactionStrength temperature =
  let
    metropolisNext iterationsNext seedNext energies magnetizations spinMatrixNext magneticFieldStrengthNext interactionStrengthNext temperatureNext =
      if iterationsNext < 0 then
        (seedNext, energies, magnetizations, spinMatrixNext)
      else
        let
          (height, width) = shape spinMatrixNext
          ((i, j), seedNextNext) = Random.step (Random.pair (Random.int 0 (height-1)) (Random.int 0 (width-1))) seedNext
          (r, seedNextNextNext) = Random.step (Random.float 0 1) seedNextNext
          dE = -2.0 * pointEnergy spinMatrixNext i j magneticFieldStrengthNext interactionStrengthNext
          dM = -2.0 * spinMagnetizationAt spinMatrixNext i j
          lastE = Maybe.withDefault 0 <| Array.get (Array.length energies - 1) energies
          lastM = Maybe.withDefault 0 <| Array.get (Array.length magnetizations - 1) magnetizations
        in
          if dE < 0 || r < e^(-dE/temperatureNext) then
            let
              spinMatrixNextNext = flipSpinAt spinMatrixNext i j
            in
              metropolisNext (iterationsNext - 1) seedNextNextNext (Array.push (lastE + dE) energies) (Array.push (lastM + dM) magnetizations) spinMatrixNextNext magneticFieldStrengthNext interactionStrengthNext temperatureNext
          else
            metropolisNext (iterationsNext - 1) seedNextNextNext (Array.push lastE energies) (Array.push lastM magnetizations) spinMatrixNext magneticFieldStrengthNext interactionStrengthNext temperatureNext
    totalEnergyNext = Array.fromList [totalEnergy spinMatrix magneticFieldStrength interactionStrength]
    totalMagnetizationNext = Array.fromList [totalMagnetization spinMatrix]
  in
    metropolisNext iterations seed totalEnergyNext totalMagnetizationNext spinMatrix magneticFieldStrength interactionStrength temperature

spinToDiv : Spin -> Html Msg
spinToDiv spin =
  case spin of
    Up -> Html.div [Attributes.class "spinUp", Attributes.style [("width", "30px"), ("height", "30px"), ("backgroundColor", "white")]] []
    Down -> Html.div [Attributes.class "spinDown", Attributes.style [("width", "30px"), ("height", "30px"), ("backgroundColor", "black")]] []

type Msg
  = NoOp
  | ChangeWidth String
  | ChangeHeight String
  | ChangeConfiguration InitialConfiguration
  | ChangeTemperature String
  | ChangeMagneticFieldStrength String
  | ChangeInteractionStrength String
  | StepMetropolis
  | RunMetropolis
  | ChangeMetropolisSteps String
  | ToggleRunning
  | UpdatePlots

type alias Model =
  { randomSeed : Random.Seed,
    spinMatrix : SpinMatrix,
    width : Int,
    height : Int,
    steps : Int,
    currentConfiguration : InitialConfiguration,
    temperature : Int,
    magneticFieldStrength : Float,
    interactionStrength : Float,
    running : Bool,
    currentStep : Int,
    totalEnergies : List (Float, Float),
    totalMagnetizations : List (Float, Float),
    avgEnergy : Float,
    avgMagnetization : Float
  }

initialModel : Model
initialModel =
  { randomSeed = Random.initialSeed 0,
    spinMatrix = first <| initMatrix (Random <| Random.initialSeed 0) 5 5,
    width = 5,
    height = 5,
    steps = 1,
    currentConfiguration = Random <| Random.initialSeed 0,
    temperature = 5,
    magneticFieldStrength = 1,
    interactionStrength = 1,
    running = False,
    currentStep = 0,
    totalEnergies = [],
    totalMagnetizations = [],
    avgEnergy = 0,
    avgMagnetization = 0
  }

floatToString f =
  let
    s = fromFloat f
    separator =
      if String.contains "." s then
        "."
      else
        ","
    parts = String.split separator s
    a = List.head parts
          |> withDefault "0"
    b = List.tail parts
          |> withDefault ["0"]
          |> String.concat
          |> String.left 3
  in
    a ++ "." ++ b

numberInputWithLabel label placeholder value msg =
  Html.tr
    []
    [ Html.td
        []
        [ Html.text label]
    , Html.td
        []
        [ Html.input
            [ Attributes.type_ "number"
            , Attributes.placeholder placeholder
            , Attributes.value <| fromInt value
            , Events.onInput msg
            ]
            []
        ]
    ]

view : Model -> Html Msg
view model =
  Html.div
    []
    [ Html.table
        [Attributes.style [("display", "box")]]
        [ numberInputWithLabel "Width: " "Width" model.width (\str -> ChangeWidth str)
        , numberInputWithLabel "Height: " "Height" model.height (\str -> ChangeHeight str)
        , numberInputWithLabel "Interaction strength: " "Interaction strength" model.interactionStrength (\str -> ChangeInteractionStrength str)
        , numberInputWithLabel "Field strength: " "Field strength" model.magneticFieldStrength (\str -> ChangeMagneticFieldStrength str)
        , numberInputWithLabel "Temperature: " "Temperature" model.temperature (\str -> ChangeTemperature str)
        , numberInputWithLabel "Steps: " "Steps" model.steps (\str -> ChangeMetropolisSteps str)
        ]
    , Html.br [] []
    , Html.button [Events.onClick (ChangeConfiguration (Random model.randomSeed))] [ Html.text "Randomize"]
    , Html.button [Events.onClick (ChangeConfiguration Ups)] [ Html.text "All up"]
    , Html.button [Events.onClick (ChangeConfiguration Downs)] [ Html.text "All down"]
    , Html.button [Events.onClick StepMetropolis] [ Html.text "Step"]
    , Html.button [Events.onClick ToggleRunning] [Html.text <| if model.running then "Stop" else "Start"]
    , Html.br [] []
    , Html.text <| "Energy: " ++ (floatToString model.avgEnergy) ++ "±" ++ (floatToString << standardDeviation << listTakeLastPercentage 0.85 <| List.map second model.totalEnergies)
    , Html.br [] []
    , Html.text <| "Magnetization: " ++ (floatToString model.avgMagnetization) ++ "±" ++ (floatToString << standardDeviation << listTakeLastPercentage 0.85 <| List.map second model.totalMagnetizations)
    , Html.br [] []
    , Html.table [] <|
        Array.toList <|
          Array.map (\r -> Html.tr [] <| Array.toList r) <|
            Array.map (\r ->  Array.map (\c -> Html.td [] [spinToDiv c]) r) model.spinMatrix
    ]

listTakeLast n list = List.drop (List.length list - n) list
listTakeLastPercentage p list = listTakeLast (round << (*) p << toFloat << List.length <| list) list

listSample : List (Float, Float) -> Int -> List (Float, Float)
listSample list points =
  let
    listSampleNext agg rest pointsNext =
      if List.length rest <= pointsNext then
        if List.length rest == 1 then
          List.concat [agg, rest]
        else
          List.concat [agg, [tupleListAverage <| List.take (List.length rest - 1) rest], listTakeLast 1 rest]
      else
        let
          ps = tupleListAverage <| List.take pointsNext rest
          aggNext = List.append agg [ps]
        in
          listSampleNext aggNext (List.drop pointsNext rest) pointsNext
  in
    listSampleNext (List.take 1 list) (List.drop 1 list) points

sampledTo points list =
  let
    sampleFrom = List.length list // points
  in
    if sampleFrom > 1 then
      listSample list sampleFrom
    else
      list

indexedSample points list =
  List.map (\(i, v) -> { index = i, value = v}) <| sampledTo points list

listAverage list = (List.foldl (+) 0 list) / (toFloat <| List.length list)
tupleListAverage list =
  let
    (x, y) = List.foldl (\(x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)) (0, 0) list
  in
    (x /  (toFloat << List.length <| list), y / (toFloat << List.length <| list))

standardDeviation list =
  let
    avg = listAverage list
    d = List.map (\v -> (v - avg)^2) list |> List.foldl (+) 0
    n = toFloat <| List.length list
  in
    sqrt <| d / (n - 1)

stepN : Int -> Model -> Model
stepN n model =
  let
    (seed, energies, magnetizations, spinMatrix) = metropolis model.spinMatrix n model.randomSeed model.magneticFieldStrength model.interactionStrength (toFloat model.temperature)
    totalEnergies_ = List.append model.totalEnergies <| Array.toList <| Array.indexedMap (\i v -> (toFloat <| model.currentStep + i, v)) energies
    totalMagnetizations_ = List.append model.totalMagnetizations <| Array.toList <| Array.indexedMap (\i v -> (toFloat <| model.currentStep + i, v)) magnetizations
    avgEnergy = listAverage <| List.map second <| listTakeLastPercentage 0.85 totalEnergies_
    avgMagnetization = listAverage <| List.map second <| listTakeLastPercentage 0.85 totalMagnetizations_
  in
    { model | spinMatrix = spinMatrix, randomSeed = seed, totalEnergies = totalEnergies_, totalMagnetizations = totalMagnetizations_, avgEnergy = avgEnergy, avgMagnetization = avgMagnetization, currentStep = model.currentStep + n}

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    ChangeWidth widthString ->
      if String.length widthString > 0 then
        case toInt widthString of
          Ok width ->
            if width < 50 then
              ({ model | width = width, spinMatrix = first <| initMatrix model.currentConfiguration model.height width }, Cmd.none)
            else
              (model, Cmd.none)
          _ ->
            (model, Cmd.none)
      else
        ({ model | width = 0 }, Cmd.none)

    ChangeHeight heightString ->
      if String.length heightString > 0 then
        case toInt heightString of
          Ok height ->
            if height < 50 then
              ({ model | height = height, spinMatrix = first <| initMatrix model.currentConfiguration height model.width }, Cmd.none)
            else
              (model, Cmd.none)
          _ ->
            (model, Cmd.none)
      else
        ({ model | height = 0 }, Cmd.none)

    ChangeTemperature temperatureString ->
      if String.length temperatureString > 0 then
        case toInt temperatureString of
          Ok temperature ->
            ({ model | temperature = temperature }, Cmd.none)
          _ ->
            (model, Cmd.none)
      else
        ({ model | temperature = 0 }, Cmd.none)

    ChangeMagneticFieldStrength magneticFieldStrengthString ->
      if String.length magneticFieldStrengthString > 0 then
        case toInt magneticFieldStrengthString of
          Ok magneticFieldStrength ->
            ({ model | magneticFieldStrength = toFloat magneticFieldStrength }, Cmd.none)
          _ ->
            (model, Cmd.none)
      else
        ({ model | magneticFieldStrength = 0 }, Cmd.none)

    ChangeInteractionStrength interactionStrengthString ->
      if String.length interactionStrengthString > 0 then
        case toInt interactionStrengthString of
          Ok interactionStrength ->
            ({ model | interactionStrength = toFloat interactionStrength }, Cmd.none)
          _ ->
            (model, Cmd.none)
      else
        ({ model | interactionStrength = 0 }, Cmd.none)

    StepMetropolis ->
      let
        modelNext = stepN model.steps model
      in
        ({ modelNext | running = False }, Cmd.none)

    RunMetropolis ->
      if model.running then
        let
          limit = 10000
        in
          if model.steps <= limit then
            (stepN model.steps model, Cmd.none)
          else
            let
              modelNext = stepN limit model
            in
              ({ modelNext | steps = limit }, Cmd.none)
      else
        (model, Cmd.none)

    ChangeMetropolisSteps steps ->
      if String.length steps > 0 then
        case toInt steps of
          Ok stepsNext ->
            ({ model | steps = stepsNext }, Cmd.none)
          _ ->
            (model, Cmd.none)
      else
        ({ model | steps = 1 }, Cmd.none)

    ChangeConfiguration configuration ->
      case configuration of
        Random r ->
          let
            (spinMatrix, seed) = initMatrix (Random model.randomSeed) model.height model.width
          in
            case seed of
              Just s -> ({ model | randomSeed = s, spinMatrix = spinMatrix, totalEnergies = [], totalMagnetizations = [], avgEnergy = 0, avgMagnetization = 0, currentStep = 0}, Cmd.none)
              _ -> (model, Cmd.none)
        _ -> ({ model | spinMatrix = first <| initMatrix configuration model.height model.width, totalEnergies = [], totalMagnetizations = [], avgEnergy = 0, avgMagnetization = 0, currentStep = 0 }, Cmd.none)

    ToggleRunning ->
      ({ model | running = not model.running }, Cmd.none)

    UpdatePlots ->
      (model, Cmd.batch [totalEnergies <| indexedSample 30 model.totalEnergies, totalMagnetizations <| indexedSample 30 model.totalMagnetizations])

    NoOp ->
      (model, Cmd.none)


app = Browser.element
  { init = (initialModel, Cmd.none)
  , update = update
  , view = view
  , subscriptions = subscriptions
  }

main =
  app

type alias Point =
  { index : Float
  , value : Float
  }

subscriptions _ =
  Sub.batch [runIsing, updatePlots]

updatePlots : Sub Msg
updatePlots = Time.every (5 * 1000) (\_ -> UpdatePlots)

runIsing : Sub Msg
runIsing =
  Time.every 1 (\_ -> RunMetropolis)

port totalEnergies : List Point -> Cmd msg
port totalMagnetizations : List Point -> Cmd msg
