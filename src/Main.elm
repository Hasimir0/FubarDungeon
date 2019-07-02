module FubarDungeon exposing (main)

import Browser exposing (..)
--import Html exposing (..)
import Element exposing (..)
import Element.Border as Border
import Element.Input as Input
import Element.Font as Font
import Element.Background as Background
import Maybe exposing (..)

type alias Model =
    { currPage : Page
    , totAccounts : Int
    }

type Page
    = SelectAccountPage
    | CreateAccountPage


type alias Account =
    { name : String
    , level : Int
    }

type Accounts
    = List Account

type alias Flags =
    ()

init : Flags -> (Model, Cmd Msg)
init _ =
    ( { currPage = SelectAccountPage
      , totAccounts = 0
      }
    , Cmd.none
    )

main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type Msg
    = NoOp
    | GotoCreateAccountPage
    | GotoSelectAccountPage

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotoCreateAccountPage ->
            ( { model | currPage = CreateAccountPage }, Cmd.none )
        
        GotoSelectAccountPage ->
            ( { model | currPage = SelectAccountPage }, Cmd.none )
        
        NoOp ->
            (model, Cmd.none)


-- VIEW

view : Model -> Browser.Document Msg
view model =
    let
        content =
            case model.currPage of
                SelectAccountPage ->
                    selectAccountView model

                CreateAccountPage ->
                    createAccountView model
    in
        { title = "FUBAR Dungeon"
        , body =
            [ layout [] <|
                content 
            ]
        }

selectAccountView : Model -> Element Msg
selectAccountView model =
    column
        [ centerX
        , centerY
        , width (px(400))
        , spacingXY 0 20
        , padding 10
        , Border.color (rgb255 0 0 0)
        , Border.width 1
        ]
        [ el
            [ centerX
            , padding 10
            , Font.bold
            , Font.size 28
            ]
            (text "FUBAR Dungeon")
        , newAccountButton
        ,noAccountsText
        ,startDelveButton
        ,(selectAccountsText model)
        ]
    

createAccountView : Model -> Element Msg
createAccountView model =
    column
        [ centerX
        , centerY
        , width (px(400))
        , spacingXY 0 20
        , padding 10
        , Border.color (rgb255 0 0 0)
        , Border.width 1
        ]
        [ el
            [ centerX
            , padding 10
            , Font.bold
            , Font.size 28
            ]
            (text "FUBAR Dungeon 2")
        , newAccountButton
        ,noAccountsText
        ,startDelveButton
        ,(selectAccountsText model)
        ]
    
    

selectAccountsText : Model -> Element Msg
selectAccountsText model =
    el  [ centerX ]
        ( text
            ( "Select at least " ++ (2 - (model.totAccounts) |> String.fromInt) ++ " more account" ++ (plural model) )
        )

plural : Model -> String
plural model =
    if model.totAccounts == 1 then
        " "
    else
        "s"

noAccountsText : Element Msg
noAccountsText =
    column
        [ centerX
        , padding 100
        ]
        [ el [centerX, padding 10] (text "There are no Player accounts! >_<")
        , el [centerX, padding 10] (text "Create more, one for each Player.")
        ]
    

newAccountButton : Element Msg
newAccountButton =
    Input.button
        [ Background.color (rgb255 143 143 143)
        , Border.color (rgb255 0 0 0)
        , Border.rounded 20
        , Border.width 1
        , Font.bold
        , Font.color (rgb255 230 230 230)
        , paddingXY 20 6
        , centerX
        , width (px 250)
        , height (px 40)
        ]
        { label = el [centerX] (text "Create New Account")
        , onPress = Just GotoCreateAccountPage
        }


startDelveButton : Element Msg
startDelveButton =
    Input.button
        [ Background.color (rgb255 143 143 143)
        , Border.color (rgb255 0 0 0)
        , Border.rounded 20
        , Border.width 1
        , Font.bold
        , Font.color (rgb255 230 230 230)
        , paddingXY 20 6
        , centerX
        , width (px 250)
        , height (px 40)
        ]
        { label = el [centerX] (text "Start New Delve")
        , onPress = Nothing
        }

