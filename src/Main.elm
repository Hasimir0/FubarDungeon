-- elm-live src/Main.elm --open


module FubarDungeon exposing (main)

import Browser exposing (document)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import List.Extra as Extra exposing (updateAt)


type alias Model =
    { currPage : Page
    , accounts : List Account
    , tempUsername : String
    , tempLevel : Int
    , totSelected : Int
    }


type Msg
    = UserClickedButtonCreateAccountInSelectAccountPage
    | UserClickedButtonCancelInCreateAccountPage
    | NewUsername String
    | NewLevel String
    | SaveAccount
    | UserChangedAccountCheck Int Bool
 --   | UserClickedButtonEditAccount Int


type Page
    = SelectAccountPage
    | CreateAccountPage


type alias Account =
    { userName : String
    , userLevel : Int
    , isSelected : Bool
    , indexID : Int
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { currPage = SelectAccountPage
      , accounts = []
      , tempUsername = ""
      , tempLevel = 0
      , totSelected = 0
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
                             , isSelected = False
                             , indexID = model.accounts |> List.length
                             }
                           ]
                , tempUsername = ""
                , tempLevel = 0
                , currPage = SelectAccountPage
              }
            , Cmd.none
            )

        UserChangedAccountCheck id check ->
            ( { model
                | accounts = model.accounts |> Extra.updateAt id (newCheckStatus check)
                , totSelected =
                    if check == True then
                        model.totSelected + 1

                    else
                        model.totSelected - 1
              }
            , Cmd.none
            )

        {- UserClickedButtonEditAccount id ->
            ( { model
                | currPage = CreateAccountPage
                , tempUsername = editAccount id
              }
            , Cmd.none
            ) -}


newCheckStatus : Bool -> Account -> Account
newCheckStatus newStatus account =
    { account | isSelected = newStatus }


{- editAccount : Model -> Int -> String
editAccount model id =
    model.accounts
        >> List.map
            (\_ account ->
                if account.indexID == id then
                    account.userName

                else
                    "Could Not Find Value"
            ) -}



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
        , if model.accounts |> List.isEmpty then
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
        (if model.totSelected < 2 then
            text ("Select at least " ++ String.fromInt (2 - model.totSelected) ++ " more account" ++ plural model)

         else
            text "You are ready to start a new delve!"
        )


plural : Model -> String
plural model =
    if model.totSelected == 1 then
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
        (model.accounts
            |> List.indexedMap
                (\_ account ->
                    row []
                        [ Input.checkbox []
                            { checked = account.isSelected
                            , icon = checkboxIcon
                            , label = Input.labelAbove [] (text "")
                            , onChange = UserChangedAccountCheck account.indexID
                            }
                        , el [ centerX, padding 10 ]
                            (text (account.userName ++ " - Level " ++ String.fromInt account.userLevel))
                        , Input.button
                            [ Background.color (rgb255 143 143 143)
                            , Border.color (rgb255 0 0 0)
                            , Border.rounded 20
                            , Border.width 1
                            , Font.bold
                            , Font.size 12
                            , Font.color (rgb255 230 230 230)
                            , centerX
                            , width (px 55)
                            , height (px 20)
                            ]
                            { label = el [ centerX ] (text "EDIT")
                            , onPress = Nothing {- Just UserClickedButtonEditAccount account.indexID -}
                            }
                        , el [] (text " ")
                        , Input.button
                            [ Background.color (rgb255 143 143 143)
                            , Border.color (rgb255 0 0 0)
                            , Border.rounded 20
                            , Border.width 1
                            , Font.bold
                            , Font.size 12
                            , Font.color (rgb255 230 230 230)
                            , centerX
                            , width (px 55)
                            , height (px 20)
                            ]
                            { label = el [ centerX ] (text "DELETE")
                            , onPress = Just UserClickedButtonCreateAccountInSelectAccountPage
                            }
                        ]
                )
        )


checkboxIcon : Bool -> Element Msg
checkboxIcon a =
    if a == True then
        el [] (text "X")

    else
        el [] (text "O")


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
