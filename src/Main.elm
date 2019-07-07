module FubarDungeon exposing (main)

--import Html exposing (..)

import Browser exposing (document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input



--import Maybe exposing (..)


type alias Model =
    { currPage : Page
    , accounts : List Account
    , tempUsername : String
    , tempLevel : Int
    }


type Msg
    = UserClickedButtonCreateAccountInSelectAccountPage
    | UserClickedButtonCancelInCreateAccountPage
    | NewUsername String
    | NewLevel String
    | SaveAccount
    | UserChangedAccountSelection Bool


type Page
    = SelectAccountPage
    | CreateAccountPage


type alias Account =
    { userName : String
    , userLevel : Int
    , isSelected : Bool
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
      , tempLevel = 0
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserClickedButtonCreateAccountInSelectAccountPage ->
            ( { model | currPage = CreateAccountPage }, Cmd.none )

        UserClickedButtonCancelInCreateAccountPage ->
            ( { model | currPage = SelectAccountPage }, Cmd.none )

        NewUsername uName ->
            ( { model | tempUsername = uName }, Cmd.none )

        NewLevel uLevel ->
            ( { model | tempLevel = Maybe.withDefault 0 (String.toInt uLevel) }, Cmd.none )

        SaveAccount ->
            ( { model
                | accounts =
                    model.accounts
                        ++ [ { userName = model.tempUsername
                             , userLevel = model.tempLevel
                             , isSelecter = False
                             }
                           ]
                , tempUsername = ""
                , tempLevel = 0
                , currPage = SelectAccountPage
              }
            , Cmd.none
            )

        UserChangedAccountSelection check ->
            List.map
                (\account ->
                    if account.isSelected == True then
                        { account | isSelecte = False }

                    else
                        { account | isSelecte = True }
                )
                model.accounts



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        content =
            case model.currPage of
                SelectAccountPage ->
                    selectAccountView model

                CreateAccountPage ->
                    createAccountView
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
        , spacingXY 0 30
        , padding 10
        , Border.color (rgb255 0 0 0)
        , Border.width 1
        ]
        [ el
            [ centerX
            , Font.bold
            , Font.size 28
            ]
            (text "FUBAR Dungeon")
        , if List.isEmpty model.accounts then
            noAccountsText

          else
            someAccountsText model
        , el [ padding 10 ] (text "")
        , newAccountButton
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
        (List.map
            (\account ->
                row []
                    [ Input.checkbox []
                        { onChange = UserChangedAccountSelection
                        , icon = iconEmpty
                        , checked = iconMarked
                        , label = Nothing
                        }
                    , el [ centerX, padding 10 ]
                        (text (account.userName ++ " - Level " ++ String.fromInt account.userLevel))
                    , el [] (text "edit")
                    , el [] (text "delete")
                    ]
            )
            model.accounts
        )


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
        , onPress = Just UserClickedButtonCreateAccountInSelectAccountPage
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


createAccountView : Element Msg
createAccountView =
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
        , el [ padding 30 ] (text "")
        , saveAccountButton
        , cancelButton
        ]


newAccount : Element Msg
newAccount =
    column
        [ centerX
        , centerY
        ]
        [ paragraph [ paddingEach { top = 10, bottom = 0, left = 0, right = 0 } ] [ text "Oh ye brave and courageous Player, write your UserNAME here!" ]
        , Input.text
            [ Border.color (rgb255 0 0 0)
            , Border.width 1
            ]
            { label = Input.labelAbove [ Font.size 18 ] (text "")
            , onChange = NewUsername
            , placeholder = Nothing
            , text = ""
            }
        , paragraph [ paddingEach { top = 50, bottom = 0, left = 0, right = 0 } ]
            [ text "New Players start at "
            , el [ Font.bold, Font.italic ] (text "Level 0!")
            ]
        , paragraph [] [ text "But if you are a veteran you can input your UserLEVEL here..." ]
        , Input.text
            [ Border.color (rgb255 0 0 0)
            , Border.width 1
            ]
            { label = Input.labelAbove [] (text "")
            , onChange = NewLevel
            , placeholder = Nothing
            , text = ""
            }
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
        , onPress = Just UserClickedButtonCancelInCreateAccountPage
        }
