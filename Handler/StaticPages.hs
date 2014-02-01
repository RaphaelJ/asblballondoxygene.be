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

getSeancesLudiquesR :: Handler Html
getSeancesLudiquesR = do
    defaultLayout $ do
        setTitle "Séances ludiques et éducatives - ASBL Ballon d'Oxygène"
        $(widgetFile "seancesludiques")

getSnoezelenR :: Handler Html
getSnoezelenR = do
    defaultLayout $ do
        setTitle "Espace snoezelen - ASBL Ballon d'Oxygène"
        $(widgetFile "snoezelen")

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
