module Main (..) where

import Html exposing (Html)
import Html.Events as Events
import Html.Attributes as Attributes
import Array
import Array exposing (Array, get, indexedMap)
import Maybe
import Maybe exposing (andThen, withDefault)
import StartApp
import StartApp.Simple
import Random
import Time
import String exposing (toInt)
import Window
import Effects

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
        spinGenerator = Random.map (\b -> if b then Up else Down) Random.bool
        spinListGenerator = Random.list width spinGenerator
        spinMatrixGenerator = Random.list height spinListGenerator
        (spinMatrix,seed) = Random.generate spinMatrixGenerator seed
      in
        (Array.map Array.fromList <| Array.fromList spinMatrix, Just seed)

flipSpinAt spinMatrix i j =
  let
    row = get i spinMatrix
    spin = row `andThen` get j
  in
    case spin of
      Just s ->
        let
          row' = Maybe.map (Array.set j (flipSpin s)) row
        in
          case row' of
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
    i' = boundaryCondition 0 width i
    j' = boundaryCondition 0 height j
  in
    withDefault 0 <| Maybe.map spinMagnetization <| get j' spinMatrix `andThen` get i'

totalMagnetization : SpinMatrix -> Float
totalMagnetization spinMatrix =
  let
    (height, width) = shape spinMatrix
    totalMagnetization' i j spinMatrix magnetization =
      if i == height - 1 && j == width - 1 then
        magnetization
      else
        let
          magnetization' = magnetization + spinMagnetizationAt spinMatrix i j
        in
          if j == width - 1 then
            totalMagnetization' (i + 1) 0 spinMatrix magnetization'
          else
            totalMagnetization' i (j + 1) spinMatrix magnetization'
  in
    totalMagnetization' 0 0 spinMatrix 0

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
    totalEnergy' spinMatrix i j magneticFieldStrength interactionStrength energy =
      let
        (height, width) = shape spinMatrix
      in
        if i == height - 1 && j == width - 1 then
          energy
        else
          let
            energy' = energy + pointEnergy spinMatrix i j magneticFieldStrength (0.5 * interactionStrength)
          in
            if j == width - 1 then
              totalEnergy' spinMatrix (i + 1) 0 magneticFieldStrength interactionStrength energy'
            else
              totalEnergy' spinMatrix i (j + 1) magneticFieldStrength interactionStrength energy'
  in
    totalEnergy' spinMatrix 0 0 magneticFieldStrength interactionStrength 0

metropolis spinMatrix iterations seed magneticFieldStrength interactionStrength temperature =
  let
    metropolis' iterations seed energyDifferences magnetizationDifferences spinMatrix magneticFieldStrength interactionStrength temperature =
      if iterations < 0 then
        (seed, energyDifferences, magnetizationDifferences, spinMatrix)
      else
        let
          (height, width) = shape spinMatrix
          ((i, j), seed') = Random.generate (Random.pair (Random.int 0 height) (Random.int 0 width)) seed
          (r, seed'') = Random.generate (Random.float 0 1) seed'
          dE = -2.0 * pointEnergy spinMatrix i j magneticFieldStrength interactionStrength
          dM = -2.0 * spinMagnetizationAt spinMatrix i j
        in
          if dE < 0 || r < e^(-dE/temperature) then
            let
              spinMatrix' = flipSpinAt spinMatrix i j
            in
              metropolis' (iterations - 1) seed'' (Array.push dE energyDifferences) (Array.push dM magnetizationDifferences) spinMatrix' magneticFieldStrength interactionStrength temperature
          else
            metropolis' (iterations - 1) seed'' (Array.push 0 energyDifferences) (Array.push 0 magnetizationDifferences) spinMatrix magneticFieldStrength interactionStrength temperature
  in
    metropolis' iterations seed Array.empty Array.empty spinMatrix magneticFieldStrength interactionStrength temperature

spinToDiv : Spin -> Html
spinToDiv spin =
  case spin of
    Up -> Html.div [Attributes.class "spinUp", Attributes.style [("width", "30px"), ("height", "30px"), ("backgroundColor", "white")]] []
    Down -> Html.div [Attributes.class "spinDown", Attributes.style [("width", "30px"), ("height", "30px"), ("backgroundColor", "black")]] []

type Action
  = NoOp
  | ChangeWidth String
  | ChangeHeight String
  | ChangeConfiguration InitialConfiguration
  | ChangeTemperature String
  | ChangeMagneticFieldStrength String
  | ChangeInteractionStrength String
  | StepOneMetropolis
  | RunMetropolis
  | ToggleRunning

type alias Model =
  { randomSeed : Random.Seed,
    spinMatrix : SpinMatrix,
    width : Int,
    height : Int,
    currentConfiguration : InitialConfiguration,
    temperature : Int,
    magneticFieldStrength : Float,
    interactionStrength : Float,
    running : Bool,
    currentStep : Int,
    energyDifferences : List (Float, Float),
    magnetizationDifferences : List (Float, Float),
    totalEnergies : List (Float, Float),
    totalMagnetizations : List (Float, Float),
    avgEnergy : Float,
    avgMagnetization : Float
  }

initialModel : Model
initialModel =
  { randomSeed = Random.initialSeed 0,
    spinMatrix = fst <| initMatrix (Random <| Random.initialSeed 0) 5 5,
    width = 5,
    height = 5,
    currentConfiguration = Random <| Random.initialSeed 0,
    temperature = 5,
    magneticFieldStrength = 1,
    interactionStrength = 1,
    running = False,
    currentStep = 0,
    energyDifferences = [],
    magnetizationDifferences = [],
    totalEnergies = [],
    totalMagnetizations = [],
    avgEnergy = 0,
    avgMagnetization = 0
  }

floatToString f =
  let
    s = toString f
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

numberInputWithLabel label placeholder value signal =
  Html.tr
    []
    [ Html.td
        []
        [ Html.text label]
    , Html.td
        []
        [ Html.input
            [ Attributes.type' "number"
            , Attributes.placeholder placeholder
            , Attributes.value <| toString value
            , Events.on "input" Events.targetValue signal
            ]
            []
        ]
    ]

view : Signal.Address Action -> Model -> Html
view address model =
  Html.div
    []
    [ Html.table
        [Attributes.style [("display", "box")]]
        [ numberInputWithLabel "Width: " "Width" model.width (\str -> Signal.message address (ChangeWidth str))
        , numberInputWithLabel "Height: " "Height" model.height (\str -> Signal.message address (ChangeHeight str))
        , numberInputWithLabel "Interaction strength: " "Interaction strength" model.interactionStrength (\str -> Signal.message address (ChangeInteractionStrength str))
        , numberInputWithLabel "Field strength: " "Field strength" model.magneticFieldStrength (\str -> Signal.message address (ChangeMagneticFieldStrength str))
        , numberInputWithLabel "Temperature: " "Temperature" model.temperature (\str -> Signal.message address (ChangeTemperature str))
        ]
    , Html.br [] []
    , Html.button [Events.onClick address (ChangeConfiguration (Random model.randomSeed))] [ Html.text "Randomize"]
    , Html.button [Events.onClick address (ChangeConfiguration Ups)] [ Html.text "All up"]
    , Html.button [Events.onClick address (ChangeConfiguration Downs)] [ Html.text "All down"]
    , Html.button [Events.onClick address StepOneMetropolis] [ Html.text "Step"]
    , Html.button [Events.onClick address ToggleRunning] [Html.text <| if model.running then "Stop" else "Start"]
    , Html.br [] []
    , Html.text <| "Energy: " ++ (floatToString model.avgEnergy) ++ "±" ++ (floatToString <| standardDeviation <| List.map snd model.totalEnergies)
    , Html.br [] []
    , Html.text <| "Magnetization: " ++ (floatToString model.avgMagnetization) ++ "±" ++ (floatToString <| standardDeviation <| List.map snd model.totalMagnetizations)
    , Html.br [] []
    , Html.table [] <|
        Array.toList <|
          Array.map (\r -> Html.tr [] <| Array.toList r) <|
            Array.map (\r ->  Array.map (\c -> Html.td [] [spinToDiv c]) r) model.spinMatrix
    ]

listTakeLast n list = List.drop (List.length list - n) list

listSample : List (Float, Float) -> Int -> List (Float, Float)
listSample list points =
  let
    listSample' agg rest points =
      if List.length rest <= points then
        if List.length rest == 1 then
          List.concat [agg, rest]
        else
          List.concat [agg, [tupleListAverage <| List.take (List.length rest - 1) rest], listTakeLast 1 rest]
      else
        let
          ps = tupleListAverage <| List.take points rest
          agg' = List.append agg [ps]
        in
          listSample' agg' (List.drop points rest) points
  in
    listSample' (List.take 1 list) (List.drop 1 list) points

sampledTo points list =
  let
    sampleFrom = List.length list // points
  in
    if sampleFrom > 1 then
      listSample list sampleFrom
    else
      list

listAverage list = (List.foldl (+) 0 list) / (toFloat <| List.length list)
tupleListAverage list =
  let
    (x, y) = List.foldl (\(x, y) (x', y') -> (x + x', y + y')) (0, 0) list
  in
    (x /  (toFloat << List.length <| list), y / (toFloat << List.length <| list))

standardDeviation list =
  let
    avg = listAverage list
    d = List.map (\v -> (v - avg)^2) list |> List.foldl (+) 0
    n = toFloat <| List.length list
  in
    sqrt <| d / (n - 1)

stepOne : Model -> Model
stepOne model =
  let
    (seed, energyDifferences, magnetizationDifferences, spinMatrix) = metropolis model.spinMatrix 1 model.randomSeed model.magneticFieldStrength model.interactionStrength (toFloat model.temperature)--metropolis 2 model.randomSeed Array.empty Array.empty model.spinMatrix model.magneticFieldStrength model.interactionStrength (toFloat model.temperature)
    energiesAppended = List.append model.energyDifferences <| List.indexedMap (\i v -> (toFloat <| model.currentStep + i, v)) <| Array.toList energyDifferences
    magnetizationsAppended = List.append model.magnetizationDifferences <| List.indexedMap (\i v -> (toFloat <| model.currentStep + i, v)) <| Array.toList magnetizationDifferences
    totalEnergy' = totalEnergy spinMatrix model.magneticFieldStrength model.interactionStrength
    totalEnergies = List.append model.totalEnergies [(toFloat model.currentStep, totalEnergy')]
    totalMagnetization' = totalMagnetization spinMatrix
    totalMagnetizations = List.append model.totalMagnetizations [(toFloat model.currentStep, totalMagnetization')]
    avgEnergy = listAverage <| List.map snd <| listTakeLast 2000 totalEnergies
    avgMagnetization = listAverage <| List.map snd <| listTakeLast 2000 totalMagnetizations
  in
    { model | spinMatrix = spinMatrix, randomSeed = seed, energyDifferences = energiesAppended, magnetizationDifferences = magnetizationsAppended, totalEnergies = totalEnergies, totalMagnetizations = totalMagnetizations, avgEnergy = avgEnergy, avgMagnetization = avgMagnetization, currentStep = model.currentStep + 1 }

update : Action -> Model -> Model
update action model =
  case action of
    ChangeWidth widthString ->
      if String.length widthString > 0 then
        case toInt widthString of
          Ok width' ->
            if width' < 50 then
              { model | width = width', spinMatrix = fst <| initMatrix model.currentConfiguration model.height width' }
            else
              model
          _ ->
            model
      else
        { model | width = 0 }

    ChangeHeight heightString ->
      if String.length heightString > 0 then
        case toInt heightString of
          Ok height' ->
            if height' < 50 then
              { model | height = height', spinMatrix = fst <| initMatrix model.currentConfiguration height' model.width }
            else
              model
          _ ->
            model
      else
        { model | height = 0 }

    ChangeTemperature temperatureString ->
      if String.length temperatureString > 0 then
        case toInt temperatureString of
          Ok temperature' ->
            { model | temperature = temperature' }
          _ ->
            model
      else
        { model | temperature = 0 }

    ChangeMagneticFieldStrength magneticFieldStrengthString ->
      if String.length magneticFieldStrengthString > 0 then
        case toInt magneticFieldStrengthString of
          Ok magneticFieldStrength' ->
            { model | magneticFieldStrength = toFloat magneticFieldStrength' }
          _ ->
            model
      else
        { model | magneticFieldStrength = 0 }

    ChangeInteractionStrength interactionStrengthString ->
      if String.length interactionStrengthString > 0 then
        case toInt interactionStrengthString of
          Ok interactionStrength' ->
            { model | interactionStrength = toFloat interactionStrength' }
          _ ->
            model
      else
        { model | interactionStrength = 0 }

    StepOneMetropolis ->
      stepOne model

    RunMetropolis ->
      if model.running then
        stepOne model
      else
        model

    ChangeConfiguration configuration' ->
      case configuration' of
        Random r ->
          let
            (spinMatrix, seed) = initMatrix (Random model.randomSeed) model.height model.width
          in
            case seed of
              Just s -> { model | randomSeed = s, spinMatrix = spinMatrix, energyDifferences = [], magnetizationDifferences = [], totalEnergies = [], totalMagnetizations = [], avgEnergy = 0, avgMagnetization = 0, currentStep = 0}
              _ -> model
        _ -> { model | spinMatrix = fst <| initMatrix configuration' model.height model.width, energyDifferences = [], magnetizationDifferences = [], totalEnergies = [], totalMagnetizations = [], avgEnergy = 0, avgMagnetization = 0, currentStep = 0 }

    ToggleRunning ->
      { model | running = not model.running }

    NoOp ->
      model

runIsing : Signal.Signal Action
runIsing =
  Signal.map (\_ -> RunMetropolis) <| Time.every <| Time.millisecond

app = StartApp.start
  { init = (initialModel, Effects.none)
  , update = \m a -> update m a |> \m -> (m, Effects.none)
  , view = view
  , inputs = [runIsing]
  }

main : Signal.Signal Html
main =
  app.html

type alias Point =
  { index : Float
  , value : Float
  }

samplingSignal = Time.every <| 5* Time.second

port energyDifferences : Signal (List Point)
port energyDifferences =
  Signal.sampleOn
    samplingSignal
    (Signal.map (\m -> List.map (\(i, v) -> { index = i, value = v}) <| sampledTo 30 m.energyDifferences) app.model)

port magnetizationDifferences : Signal (List Point)
port magnetizationDifferences =
  Signal.sampleOn
    samplingSignal
    (Signal.map (\m -> List.map (\(i, v) -> { index = i, value = v}) <| sampledTo 30 m.magnetizationDifferences) app.model)

port totalEnergies: Signal (List Point)
port totalEnergies=
  Signal.sampleOn
    samplingSignal
    (Signal.map (\m -> List.map (\(i, v) -> { index = i, value = v}) <| sampledTo 30 m.totalEnergies) app.model)

port totalMagnetizations: Signal (List Point)
port totalMagnetizations =
  Signal.sampleOn
    samplingSignal
    (Signal.map (\m -> List.map (\(i, v) -> { index = i, value = v}) <| sampledTo 30 m.totalMagnetizations) app.model)
