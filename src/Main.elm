port module Main exposing (main)

import Browser
import Diff
import Html exposing (button, div, h5, input, label, span, text)
import Html.Attributes exposing (checked, class, for, id, style, type_)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Zipper as Zipper
import Platform.Cmd as Cmd


type Side
    = Front
    | Back


type alias CardSide =
    { content : String, status : CardStatus }


type alias Card =
    { front : CardSide, back : CardSide }


type alias SavedModel =
    { allCards : List Card
    , showFirst : Side
    , sessionNumber : Int
    }


type alias Model =
    { deck : Maybe (Zipper.Zipper Card)
    , allCards : List Card
    , sideShowing : Side
    , guess : String
    , diff : Maybe (List (Diff.Change Char))
    , showFirst : Side
    , sessionNumber : Int
    }


type Correctness
    = Again
    | Correct
    | Easy


type CardStatus
    = New
    | Learning Int -- count of continuous correct guesses
    | Graduated Int -- session number it originally graduated


type Msg
    = NoOp
    | Flip
    | Next Correctness
    | UpdateGuess String
    | ToggleShowFrontFirst
    | ShowDiff


port setStorage : Encode.Value -> Cmd msg


main : Program Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    let
        allCards =
            List.map
                (\{ front, back } ->
                    { front = { content = front, status = New }
                    , back = { content = back, status = New }
                    }
                )
                cardList

        defaultModel =
            { deck = allCards |> Zipper.fromList -- TODO fix to pull the cards we want
            , allCards = allCards
            , sideShowing = Front
            , guess = ""
            , diff = Nothing
            , showFirst = Front
            , sessionNumber = 0
            }
    in
    ( case Decode.decodeValue decoder flags of
        Ok savedModel ->
            { defaultModel
                | sessionNumber = savedModel.sessionNumber + 1
                , showFirst = savedModel.showFirst
                , allCards = savedModel.allCards
            }

        Err _ ->
            defaultModel
    , Cmd.none
    )


view : Model -> Html.Html Msg
view model =
    let
        deck =
            model.deck

        currentCard =
            Maybe.map Zipper.current deck

        nextButton correctness =
            div [ class "col" ]
                [ button
                    [ class ("btn btn-" ++ correctnessTheme correctness ++ " py-4 rounded-0 w-100")
                    , style "text-align" "center"
                    , onClick (Next correctness)
                    ]
                    [ text (displayCorrectness correctness) ]
                ]

        card =
            case currentCard of
                Nothing ->
                    text "That's all, folks!"

                Just presentCard ->
                    div [ class "card h-100", onClick Flip ]
                        [ div [ class "text-center d-flex align-items-center justify-content-center" ]
                            [ h5 [ class "card-title" ] [ text (cardShowing presentCard model.sideShowing) ] ]
                        ]
    in
    div [ class "container-fluid vh-100 d-flex flex-column" ]
        [ div [ class "row h-50 m-2" ]
            [ div [ class "col" ]
                [ card ]
            ]
        , div [ class "row m-2" ]
            [ input [ Html.Attributes.value model.guess, onInput UpdateGuess ] [] ]
        , div [ class "m-2" ] (viewDiff model.diff)
        , div [ class "row row-cols-3 m-2 gx-2" ]
            [ nextButton Again
            , nextButton Correct
            , nextButton Easy
            ]
        , div [ class "m-2 mt-5" ]
            [ label [ for "front-first", class "me-3" ] [ text "Show front first?" ]
            , input
                [ id "front-first"
                , type_ "checkbox"
                , checked (sideToShowFrontFirst model.showFirst)
                , onInput (\_ -> ToggleShowFrontFirst)
                ]
                []
            ]
        ]


viewDiff : Maybe (List (Diff.Change Char)) -> List (Html.Html Msg)
viewDiff diff =
    let
        viewCharDiff d =
            case d of
                Diff.Added c ->
                    span [ class "bg-success text-nowrap p-1 font-monospace" ] [ text (String.fromChar c) ]

                Diff.Removed c ->
                    span [ class "bg-danger text-nowrap p-1 font-monospace" ] [ text (String.fromChar c) ]

                Diff.NoChange c ->
                    span [ class "text-nowrap p-1 font-monospace" ] [ text (String.fromChar c) ]
    in
    case diff of
        Nothing ->
            [ span [ class "text-nowrap p-1 font-monospace" ] [ text "" ] ]

        Just presentDiff ->
            List.map viewCharDiff presentDiff


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Flip ->
            update ShowDiff { model | sideShowing = flip model.sideShowing }

        Next selectedCorrectness ->
            let
                newModel =
                    -- TODO update deck's end based on Next decision
                    { model
                        | guess = ""
                        , diff = Nothing
                        , sideShowing = model.showFirst
                        , deck =
                            model.deck
                                |> Maybe.andThen Zipper.next
                        , allCards =
                            case model.deck of
                                Nothing ->
                                    model.allCards

                                Just presentDeck ->
                                    let
                                        currentCard =
                                            Zipper.current
                                                presentDeck

                                        answerSide =
                                            flip model.showFirst
                                    in
                                    updateCards
                                        (handleNextCardSide model.sessionNumber selectedCorrectness)
                                        answerSide
                                        currentCard
                                        model.allCards
                    }
            in
            ( newModel, saveModel newModel )

        UpdateGuess s ->
            let
                showingFirstSide =
                    model.showFirst == model.sideShowing
            in
            update
                (if showingFirstSide then
                    NoOp

                 else
                    ShowDiff
                )
                { model | guess = s }

        ToggleShowFrontFirst ->
            let
                newModel =
                    { model
                        | showFirst = flip model.showFirst
                        , sideShowing = flip model.showFirst
                    }
            in
            ( newModel, saveModel newModel )

        ShowDiff ->
            ( { model
                | diff =
                    Maybe.map
                        (\currentCard ->
                            Diff.diff (String.toList model.guess)
                                (flip model.showFirst
                                    |> cardShowing currentCard
                                    |> String.toList
                                )
                        )
                        (Maybe.map Zipper.current model.deck)
              }
            , Cmd.none
            )


saveModel : Model -> Cmd msg
saveModel model =
    model
        |> toSavedModel
        |> encode
        |> setStorage


handleNextCardSide : Int -> Correctness -> CardSide -> CardSide
handleNextCardSide sessionNumber correctness cardSide =
    { cardSide | status = handleNext sessionNumber correctness cardSide.status }


handleNext : Int -> Correctness -> CardStatus -> CardStatus
handleNext sessionNumber selectedCorrectness oldStatus =
    case ( selectedCorrectness, oldStatus ) of
        ( Again, _ ) ->
            Learning 0

        ( _, New ) ->
            Learning 0

        ( _, Graduated n ) ->
            Graduated n

        ( Easy, Learning _ ) ->
            Graduated sessionNumber

        ( Correct, Learning continuousCorrectSoFar ) ->
            if continuousCorrectSoFar == 2 then
                Graduated sessionNumber

            else
                Learning (continuousCorrectSoFar + 1)



-- requirements:
-- don't save all wrong ones for the end
-- not time-based, session/card-based
-- distinguish sessions and number of cards
--
-- options:
-- new cards/session
-- learning/graduated for a given card; graduated rarer
-- "ease factor": what I chose last
--
-- X number of starting cards for a session
-- a card is either new, learning, graduated - display total count for each
----      rarity --->
--
-- new: show all new right away, shifts out once you hit anything, always new -> learning
--
-- learning: correct 3 times in a row or easy 1 time -> graduated
-- show a learning card 3x per session.
--
-- graduated: come back all together after skipping a session, mixed in with learning cards:
-- show only once per session
-- wrong -> learning
-- day 1: 5 cards from learning -> graduated
-- day 2: 4 cards from learning -> graduated, showing no graduated cards
-- day 3: X cards from learning -> graduated, showing the 5 graduated cards from day 1
-- day 4: Y cards from learning -> graduated, showing the 4 graduated cards from day 2
-- day 5: Z cards from learning -> graduated, showing the 5 + X cards from days 1 and 3


displayCorrectness : Correctness -> String
displayCorrectness c =
    case c of
        Again ->
            "again"

        Correct ->
            "correct"

        Easy ->
            "easy"


correctnessTheme : Correctness -> String
correctnessTheme c =
    case c of
        Again ->
            "danger"

        Correct ->
            "success"

        Easy ->
            "primary"


cardShowing : Card -> Side -> String
cardShowing card side =
    case side of
        Front ->
            card.front.content

        Back ->
            card.back.content


flip : Side -> Side
flip s =
    case s of
        Front ->
            Back

        Back ->
            Front


showFrontFirstToSide : Bool -> Side
showFrontFirstToSide showFrontFirst =
    if showFrontFirst then
        Front

    else
        Back


sideToShowFrontFirst : Side -> Bool
sideToShowFrontFirst side =
    case side of
        Front ->
            True

        Back ->
            False


updateCards : (CardSide -> CardSide) -> Side -> Card -> List Card -> List Card
updateCards f side toUpdate =
    List.map
        (\card ->
            if cardShowing toUpdate side == cardShowing card side then
                case side of
                    Front ->
                        { card | front = f card.front }

                    Back ->
                        { card | back = f card.back }

            else
                card
        )


cardList : List { front : String, back : String }
cardList =
    [ { front = "1", back = "a" }, { front = "2", back = "b" }, { front = "3", back = "c" } ]



-- [ { front = "a", back = "PARTICLE: (emphasis, emotion or confirmation)" }, { front = "akesi", back = "NOUN: non-cute animal; reptile, amphibian" }, { front = "ala", back = "ADJECTIVE: no, not, zero" }, { front = "alasa", back = "VERB: to hunt, forage" }, { front = "ale", back = "ADJECTIVE: all; abundant, countless, bountiful, every, plentiful\nNOUN: abundance, everything, life, universe\nNUMBER: 100" }, { front = "anpa", back = "ADJECTIVE: bowing down, downward, humble, lowly, dependent" }, { front = "ante", back = "ADJECTIVE: different, altered, changed, other" }, { front = "anu", back = "PARTICLE: or" }, { front = "awen", back = "ADJECTIVE: enduring, kept, protected, safe, waiting, staying\nPRE-VERB: to continue to" }, { front = "e", back = "PARTICLE: (before the direct object)" }, { front = "en", back = "PARTICLE: (between multiple subjects)" }, { front = "esun", back = "NOUN: market, shop, fair, bazaar, business transaction" }, { front = "ijo", back = "NOUN: thing, phenomenon, object, matter" }, { front = "ike", back = "ADJECTIVE: bad, negative; non-essential, irrelevant" }, { front = "ilo", back = "NOUN: tool, implement, machine, device" }, { front = "insa", back = "NOUN: centre, content, inside, between; internal organ, stomach" }, { front = "jaki", back = "ADJECTIVE: disgusting, obscene, sickly, toxic, unclean, unsanitary" }, { front = "jan", back = "NOUN: human being, person, somebody" }, { front = "jelo", back = "ADJECTIVE: yellow, yellowish" }, { front = "jo", back = "VERB: to have, carry, contain, hold" }, { front = "kala", back = "NOUN: fish, marine animal, sea creature" }, { front = "kalama", back = "VERB: to produce a sound; recite, utter aloud" }, { front = "kama", back = "ADJECTIVE: arriving, coming, future, summoned\nPRE-VERB: to become, manage to, succeed in" }, { front = "kasi", back = "NOUN: plant, vegetation; herb, leaf" }, { front = "ken", back = "ADJECTIVE: possible\nPRE-VERB: to be able to, be allowed to, can, may" }, { front = "kepeken", back = "PREPOSITION: to use, with, by means of" }, { front = "kili", back = "NOUN: fruit, vegetable, mushroom" }, { front = "kiwen", back = "NOUN: hard object, metal, rock, stone" }, { front = "ko", back = "NOUN: clay, clinging form, dough, semi-solid, paste, powder" }, { front = "kon", back = "NOUN: air, breath; essence, spirit; hidden reality, unseen agent" }, { front = "kule", back = "ADJECTIVE: colourful, pigmented, painted" }, { front = "kulupu", back = "NOUN: community, company, group, nation, society, tribe" }, { front = "kute", back = "NOUN: ear\nVERB: to hear, listen; pay attention to, obey" }, { front = "la", back = "PARTICLE: (between the context phrase and the main sentence)" }, { front = "lape", back = "ADJECTIVE: sleeping, resting" }, { front = "laso", back = "ADJECTIVE: blue, green" }, { front = "lawa", back = "NOUN: head, mind\nVERB: to control, direct, guide, lead, own, plan, regulate, rule" }, { front = "len", back = "NOUN: cloth, clothing, fabric, textile; cover, layer of privacy" }, { front = "lete", back = "ADJECTIVE: cold, cool; uncooked, raw" }, { front = "li", back = "PARTICLE: (between any subject except mi alone or sina alone and its verb; also to introduce a new verb for the same subject)" }, { front = "lili", back = "ADJECTIVE: little, small, short; few; a bit; young" }, { front = "linja", back = "NOUN: long and flexible thing; cord, hair, rope, thread, yarn" }, { front = "lipu", back = "NOUN: flat object; book, document, card, paper, record, website" }, { front = "loje", back = "ADJECTIVE: red, reddish" }, { front = "lon", back = "PREPOSITION: located at, present at, real, true, existing" }, { front = "luka", back = "NOUN: arm, hand, tactile organ\nNUMBER: five" }, { front = "lukin", back = "NOUN: eye\nPRE-VERB: to seek, look for, try to\nVERB: to look at, see, examine, observe, read, watch" }, { front = "lupa", back = "NOUN: door, hole, orifice, window" }, { front = "ma", back = "NOUN: earth, land; outdoors, world; country, territory; soil" }, { front = "mama", back = "NOUN: parent, ancestor; creator, originator; caretaker, sustainer" }, { front = "mani", back = "NOUN: money, cash, savings, wealth; large domesticated animal" }, { front = "meli", back = "NOUN: woman, female, feminine person; wife" }, { front = "mi", back = "NOUN: I, me, we, us" }, { front = "mije", back = "NOUN: man, male, masculine person; husband" }, { front = "moku", back = "VERB: to eat, drink, consume, swallow, ingest" }, { front = "moli", back = "ADJECTIVE: dead, dying" }, { front = "monsi", back = "NOUN: back, behind, rear" }, { front = "mu", back = "PARTICLE: (animal noise or communication)" }, { front = "mun", back = "NOUN: moon, night sky object, star" }, { front = "musi", back = "ADJECTIVE: artistic, entertaining, frivolous, playful, recreational" }, { front = "mute", back = "ADJECTIVE: many, a lot, more, much, several, very\nNOUN: quantity" }, { front = "nanpa", back = "NOUN: numbers\nPARTICLE: -th (ordinal number)" }, { front = "nasa", back = "ADJECTIVE: unusual, strange; foolish, crazy; drunk, intoxicated" }, { front = "nasin", back = "NOUN: way, custom, doctrine, method, path, road" }, { front = "nena", back = "NOUN: bump, button, hill, mountain, nose, protuberance" }, { front = "ni", back = "ADJECTIVE: that, this" }, { front = "nimi", back = "NOUN: name, word" }, { front = "noka", back = "NOUN: foot, leg, organ of locomotion; bottom, lower part" }, { front = "o", back = "PARTICLE: hey! O! (vocative or imperative)" }, { front = "olin", back = "VERB: to love, have compassion for, respect, show affection to" }, { front = "ona", back = "NOUN: he, she, it, they" }, { front = "open", back = "VERB: to begin, start; open; turn on" }, { front = "pakala", back = "ADJECTIVE: botched, broken, damaged, harmed, messed up" }, { front = "pali", back = "VERB: to do, take action on, work on; build, make, prepare" }, { front = "palisa", back = "NOUN: long hard thing; branch, rod, stick" }, { front = "pan", back = "NOUN: cereal, grain; barley, corn, oat, rice, wheat; bread, pasta" }, { front = "pana", back = "VERB: to give, send, emit, provide, put, release" }, { front = "pi", back = "PARTICLE: of" }, { front = "pilin", back = "ADJECTIVE: feeling (an emotion, a direct experience)\nNOUN: heart (physical or emotional)" }, { front = "pimeja", back = "ADJECTIVE: black, dark, unlit" }, { front = "pini", back = "ADJECTIVE: ago, completed, ended, finished, past" }, { front = "pipi", back = "NOUN: bug, insect, ant, spider" }, { front = "poka", back = "NOUN: hip, side; next to, nearby, vicinity" }, { front = "poki", back = "NOUN: container, bag, bowl, box, cup, cupboard, drawer, vessel" }, { front = "pona", back = "ADJECTIVE: good, positive, useful; friendly, peaceful; simple" }, { front = "pu", back = "ADJECTIVE: interacting with the Toki Pona book" }, { front = "sama", back = "ADJECTIVE: same, similar; each other; sibling, peer, fellow\nPREPOSITION: as, like" }, { front = "seli", back = "ADJECTIVE: fire; cooking element, chemical reaction, heat source" }, { front = "selo", back = "NOUN: outer form, outer layer; bark, peel, shell, skin; boundary" }, { front = "seme", back = "PARTICLE: what? which?" }, { front = "sewi", back = "ADJECTIVE: awe-inspiring, divine, sacred, supernatural\nNOUN: area above, highest part, something elevated" }, { front = "sijelo", back = "NOUN: body (of person or animal), physical state, torso" }, { front = "sike", back = "ADJECTIVE: of one year\nNOUN: round or circular thing; ball, circle, cycle, sphere, wheel" }, { front = "sin", back = "ADJECTIVE: new, fresh; additional, another, extra" }, { front = "sina", back = "NOUN: you" }, { front = "sinpin", back = "NOUN: face, foremost, front, wall" }, { front = "sitelen", back = "NOUN: image, picture, representation, symbol, mark, writing" }, { front = "sona", back = "PRE-VERB: to know how to\nVERB: to know, be skilled in, be wise about, have information on" }, { front = "soweli", back = "NOUN: animal, beast, land mammal" }, { front = "suli", back = "ADJECTIVE: big, heavy, large, long, tall; important; adult" }, { front = "suno", back = "NOUN: sun; light, brightness, glow, radiance, shine; light source" }, { front = "supa", back = "NOUN: horizontal surface, thing to put or rest something on" }, { front = "suwi", back = "ADJECTIVE: sweet, fragrant; cute, innocent, adorable" }, { front = "tan", back = "PREPOSITION: by, from, because of" }, { front = "taso", back = "ADJECTIVE: only\nPARTICLE: but, however" }, { front = "tawa", back = "ADJECTIVE: moving\nPREPOSITION: going to, toward; for; from the perspective of" }, { front = "telo", back = "NOUN: water, liquid, fluid, wet substance; beverage" }, { front = "tenpo", back = "NOUN: time, duration, moment, occasion, period, situation" }, { front = "toki", back = "VERB: to communicate, say, speak, say, talk, use language, think" }, { front = "tomo", back = "NOUN: indoor space; building, home, house, room" }, { front = "tu", back = "NUMBER: two" }, { front = "unpa", back = "VERB: to have sexual or marital relations with" }, { front = "uta", back = "NOUN: mouth, lips, oral cavity, jaw" }, { front = "utala", back = "VERB: to battle, challenge, compete against, struggle against" }, { front = "walo", back = "ADJECTIVE: white, whitish; light-coloured, pale" }, { front = "wan", back = "ADJECTIVE: unique, united\nNUMBER: one" }, { front = "waso", back = "NOUN: bird, flying creature, winged animal" }, { front = "wawa", back = "ADJECTIVE: strong, powerful; confident, sure; energetic, intense" }, { front = "weka", back = "ADJECTIVE: absent, away, ignored" }, { front = "wile", back = "PRE-VERB: must, need, require, should, want, wish" } ]


toSavedModel : Model -> SavedModel
toSavedModel model =
    { allCards = model.allCards
    , showFirst = model.showFirst
    , sessionNumber = model.sessionNumber
    }


encode : SavedModel -> Encode.Value
encode model =
    Encode.object
        [ ( "showFrontFirst", Encode.bool (sideToShowFrontFirst model.showFirst) )
        , ( "sessionNumber", Encode.int model.sessionNumber )
        , ( "allCards", Encode.list encodeCard model.allCards )
        ]


encodeCard : Card -> Encode.Value
encodeCard card =
    Encode.object
        [ ( "front", encodeCardSide card.front )
        , ( "back", encodeCardSide card.back )
        ]


encodeCardSide : CardSide -> Encode.Value
encodeCardSide cardSide =
    Encode.object
        [ ( "content", Encode.string cardSide.content )
        , ( "status", encodeStatus cardSide.status )
        ]


encodeStatus : CardStatus -> Encode.Value
encodeStatus status =
    case status of
        New ->
            Encode.object [ ( "tag", Encode.string "New" ) ]

        Learning correctCount ->
            Encode.object [ ( "tag", Encode.string "Learning" ), ( "correctCount", Encode.int correctCount ) ]

        Graduated sessionGraduated ->
            Encode.object [ ( "tag", Encode.string "Graduated" ), ( "sessionGraduated", Encode.int sessionGraduated ) ]


decoder : Decode.Decoder SavedModel
decoder =
    Decode.map3 SavedModel
        (Decode.field "allCards" (Decode.list decodeCard))
        (Decode.field "showFrontFirst" (Decode.map showFrontFirstToSide Decode.bool))
        (Decode.field "sessionNumber" Decode.int)


decodeCard : Decode.Decoder Card
decodeCard =
    Decode.map2 Card
        (Decode.field "front" decodeCardSide)
        (Decode.field "back" decodeCardSide)


decodeCardSide : Decode.Decoder CardSide
decodeCardSide =
    Decode.map2 CardSide
        (Decode.field "content" Decode.string)
        (Decode.field "status" decodeStatus)


decodeStatus : Decode.Decoder CardStatus
decodeStatus =
    let
        decodeFromTag tag =
            case tag of
                "New" ->
                    Decode.succeed New

                "Learning" ->
                    Decode.map Learning (Decode.field "correctCount" Decode.int)

                "Graduated" ->
                    Decode.map Graduated (Decode.field "sessionGraduated" Decode.int)

                _ ->
                    Decode.fail "tag must be one of \"New\", \"Learning\", or \"Graduated\""
    in
    Decode.field "tag" Decode.string
        |> Decode.andThen decodeFromTag
