{-# LANGUAGE OverloadedStrings #-}
module Handler.StaticPages where

import Import

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "ASBL Ballon d'Oxygène"
        $(widgetFile "home")

getServicesR :: Handler Html
getServicesR = do
    defaultLayout $ do
        setTitle "Services proposés - ASBL Ballon d'Oxygène"
        $(widgetFile "services")

getEquitationAdaptR :: Handler Html
getEquitationAdaptR = do
    defaultLayout $ do
        setTitle "Équitation adaptée - ASBL Ballon d'Oxygène"
        $(widgetFile "equitationadapt")

getStagesMultiActsR :: Handler Html
getStagesMultiActsR = do
    defaultLayout $ do
        setTitle "Stages multi-activités - ASBL Ballon d'Oxygène"
        $(widgetFile "stagesmultiacts")

getSnoezelenR :: Handler Html
getSnoezelenR = do
    defaultLayout $ do
        setTitle "Espace snoezelen - ASBL Ballon d'Oxygène"
        $(widgetFile "snoezelen")

getSnoezelenChezVousR :: Handler Html
getSnoezelenChezVousR = do
    defaultLayout $ do
        setTitle "Snozelen chez vous - ASBL Ballon d'Oxygène"
        $(widgetFile "snoezelenchezvous")

getPsychomotriciteR :: Handler Html
getPsychomotriciteR = do
    defaultLayout $ do
        setTitle "Psychomotricite - ASBL Ballon d'Oxygène"
        $(widgetFile "psychomotricite")

getExpressionCorporelleMusicaleR :: Handler Html
getExpressionCorporelleMusicaleR = do
    defaultLayout $ do
        setTitle "Expression corporelle et musicale - ASBL Ballon d'Oxygène"
        $(widgetFile "expressioncorporellemusicale")

getTravaillerDansNosLocauxR :: Handler Html
getTravaillerDansNosLocauxR = do
    defaultLayout $ do
        setTitle "Travailler dans nos locaux - ASBL Ballon d'Oxygène"
        $(widgetFile "travaillerdansnoslocaux")

getAccompagnementPersoR :: Handler Html
getAccompagnementPersoR = do
    defaultLayout $ do
        setTitle "Accompagnement en situation de vie - ASBL Ballon d'Oxygène"
        $(widgetFile "accompagnementperso")

getEquipeR :: Handler Html
getEquipeR = do
    defaultLayout $ do
        setTitle "Équipe éducative et bénévolat - ASBL Ballon d'Oxygène"
        $(widgetFile "equipe")

getFinancesR :: Handler Html
getFinancesR = do
    defaultLayout $ do
        setTitle "Reconnaissance, soutien et sponsoring - ASBL Ballon d'Oxygène"
        $(widgetFile "finances")

getContactsR :: Handler Html
getContactsR = do
    defaultLayout $ do
        setTitle "Contacts et statuts - ASBL Ballon d'Oxygène"
        $(widgetFile "contacts")

getLiensR :: Handler Html
getLiensR = do
    defaultLayout $ do
        setTitle "Liens - ASBL Ballon d'Oxygène"
        $(widgetFile "liens")
