module FubarDungeon exposing (main)

--import Html exposing (..)

import Browser exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Maybe exposing (..)


type alias Model =
    { currPage : Page
    , accounts : List Account
    , tempUsername : String
    , tempLevel : String
    }


type Page
    = SelectAccountPage
    | CreateAccountPage


type alias Account =
    { userName : String
    , userLevel : String
    }


type Accounts
    = List Account


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { currPage = SelectAccountPage
      , accounts = []
      , tempUsername = ""
      , tempLevel = ""
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
    | NewUsername String
    | NewLevel String
    | SaveAccount



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
            ( model, Cmd.none )

        NewUsername uName ->
            ( { model | tempUsername = uName }, Cmd.none )

        NewLevel uLevel ->
            ( { model | tempLevel = uLevel }, Cmd.none )

        SaveAccount ->
            ( { model
                | accounts = model.accounts ++ [ { userName = model.tempUsername, userLevel = model.tempLevel } ]
                , tempUsername = ""
                , tempLevel = ""
                , currPage = SelectAccountPage
              }
            , Cmd.none
            )



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
        , width (px 400)
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
        , if List.isEmpty model.accounts then
            noAccountsText

          else
            someAccountsText model
        , startDelveButton
        , selectAccountsText model
        ]


selectAccountsText : Model -> Element Msg
selectAccountsText model =
    el [ centerX ]
        (text
            ("Select at least " ++ (2 - List.length model.accounts |> String.fromInt) ++ " more account" ++ plural model)
        )


plural : Model -> String
plural model =
    if List.length model.accounts == 1 then
        " "

    else
        "s"


noAccountsText : Element Msg
noAccountsText =
    column
        [ centerX
        , padding 100
        ]
        [ el [ centerX, padding 10 ] (text "There are no Player accounts! >_<")
        , el [ centerX, padding 10 ] (text "Create more!")
        , el [ centerX, padding 10 ] (text "At least one for each Player.")
        ]


someAccountsText : Model -> Element Msg
someAccountsText model =
    column
        [ centerX
        , padding 100
        ]
        (List.map (\account -> el [ centerX, padding 10 ] (text account.userName)) model.accounts)


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
        { label = el [ centerX ] (text "Create New Account")
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
        { label = el [ centerX ] (text "Start New Delve")
        , onPress = Nothing
        }


createAccountView : Model -> Element Msg
createAccountView model =
    column
        [ centerX
        , centerY
        , width (px 400)
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
        , newAccount
        , saveAccountButton
        , cancelButton
        ]


newAccount : Element Msg
newAccount =
    column
        [ centerX
        , centerY
        ]
        [ Input.text
            [ Border.color (rgb255 0 0 0)
            , Border.width 1
            ]
            { label = Input.labelAbove [] (text "Write your PLAYER name here...")
            , onChange = NewUsername
            , placeholder = Nothing
            , text = ""
            }
        , el [ padding 30 ] (text "")
        , Input.text
            [ Border.color (rgb255 0 0 0)
            , Border.width 1
            ]
            { label = Input.labelAbove [] (text "Write your PLAYER level here...")
            , onChange = NewLevel
            , placeholder = Nothing
            , text = ""
            }
        , el [ padding 30 ] (text "")
        ]


saveAccountButton : Element Msg
saveAccountButton =
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
        { label = el [ centerX ] (text "Save Account")
        , onPress = Just SaveAccount
        }


cancelButton : Element Msg
cancelButton =
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
        { label = el [ centerX ] (text "Cancel")
        , onPress = Just GotoSelectAccountPage
        }
