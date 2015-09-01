{-# LANGUAGE OverloadedStrings #-}

module Handler.Admin where

import Import

import qualified Data.ByteString.Lazy as LBS
import Data.Conduit.Binary (sinkLbs)
import Data.List (cycle)
import Data.Ratio
import qualified Data.Text as T
import System.Directory (removeFile)

import qualified Vision.Image as I
import qualified Vision.Image.Storage.DevIL as I
import Vision.Primitive

sessionKey :: Text
sessionKey = "CONNECTED"

getAdminLoginR :: Handler Html
getAdminLoginR = do
    redirectIfConnected

    (widget, enctype) <- generateFormPost loginForm

    let err = Nothing :: Maybe Text
    defaultLayout $ do
        setTitle "Connexion à l'administration"
        $(widgetFile "admin-login")

postAdminLoginR :: Handler Html
postAdminLoginR = do
    redirectIfConnected

    ((result, widget), enctype) <- runFormPost loginForm

    validPass <- getsYesod (appPassword . appSettings)
    case result of
        FormSuccess pass | pass == validPass -> do
            setSession sessionKey ""
            redirect AdminAlbumsR
        _ -> do
            let err = Just "Mot de passe invalide." :: Maybe Text
            defaultLayout $ do
                setTitle "Connexion à l'administration"
                $(widgetFile "admin-login")

loginForm :: Form Text
loginForm = renderDivs $ areq passwordField "Mot de passe" Nothing

redirectIfConnected :: Handler ()
redirectIfConnected = do
    v <- lookupSession sessionKey
    case v of
        Just _  -> redirect AdminAlbumsR
        Nothing -> return ()

redirectIfNotConnected :: Handler ()
redirectIfNotConnected = do
    v <- lookupSession sessionKey
    case v of
        Just _  -> return ()
        Nothing -> redirect AdminLoginR

-- Albums ----------------------------------------------------------------------

getAdminAlbumsR :: Handler Html
getAdminAlbumsR = do
    redirectIfNotConnected

    albums <- runDB $ selectList [] [Asc AlbumName]
    (widget, enctype) <- generateFormPost albumForm

    let err = Nothing :: Maybe Text
    defaultLayout $ do
        setTitle "Liste des albums"
        $(widgetFile "admin-albums")

postAdminAlbumsR :: Handler Html
postAdminAlbumsR = do
    redirectIfNotConnected

    albums <- runDB $ selectList [] [Asc AlbumName]
    ((result, widget), enctype) <- runFormPost albumForm

    err <- case result of
        FormSuccess album -> do
            m <- runDB $ insertUnique album
            case m of
                Just _  -> redirect AdminAlbumsR
                Nothing -> return $! Just ("Album existant." :: Text)
        _ -> return Nothing

    defaultLayout $ do
        setTitle "Liste des albums"
        $(widgetFile "admin-albums")

albumForm :: Form Album
albumForm html = flip renderDivs html $ Album
        <$> areq textField "Nom de l'album : " Nothing

-- Album -----------------------------------------------------------------------

getAdminAlbumR :: AlbumId -> Handler Html
getAdminAlbumR albumId = do
    redirectIfNotConnected

    (album, pics) <- runDB $ do
        album <- get404 albumId
        pics  <- selectList [PictureAlbum ==. albumId] [Asc PictureId]
        return (album, pics)
    (widget, enctype) <- generateFormPost pictureForm

    let err = Nothing :: Maybe Text
        pics' = zip (cycle [1, 2, 3, 4 :: Int]) pics
    defaultLayout $ do
        setTitle "Gérer l'album"
        $(widgetFile "admin-album")

postAdminAlbumR :: AlbumId -> Handler Html
postAdminAlbumR albumId = do
    redirectIfNotConnected

    ((result, widget), enctype) <- runFormPost pictureForm

    (album, pics) <- runDB $ do
        album <- get404 albumId
        pics  <- selectList [PictureAlbum ==. albumId] [Asc PictureId]
        return (album, pics)

    err <- case result of
        FormSuccess info -> do
            mErr <- processImage info
            case mErr of
                Just err -> return $ Just $ T.pack $ show err
                Nothing  -> redirect $ AdminAlbumR albumId
        _ -> return Nothing

    let pics' = zip (cycle [1, 2, 3, 4 :: Int]) pics
    defaultLayout $ do
        setTitle "Gérer l'album"
        $(widgetFile "admin-album")
  where
    processImage (desc, file) = do
        app <- getYesod

        lbs  <- fileSource file $$ sinkLbs

        case I.loadBS I.Autodetect (LBS.toStrict lbs) of
            Right (rgb :: I.RGB) -> do
                let mini = resizeImage rgb

                runDB $ do
                    picId <- insert (Picture albumId desc)
                    _ <- liftIO $
                        I.save I.Autodetect (picturePath app picId)   rgb >>
                        I.save I.Autodetect (miniaturePath app picId) mini
                    return ()

                return Nothing
            Left ioErr  -> return $ Just $ T.pack $ show ioErr

    resizeImage img =
        let Z :. h  :. w  = I.shape img
            Z :. h' :. w' = miniatureSize
            -- Redimensionne l'image sur la dimension la plus petite.
            ratio = min (w % w') (h % h')
            (tmpW, tmpH) =
                (truncate ((w % 1) / ratio), truncate ((h % 1) / ratio))
            tmp = I.resize I.Bilinear (Z :. tmpH :. tmpW) img :: I.RGBDelayed
            -- Coupe l'image sur la partie centrale.
            rect = Rect ((tmpW - w') `div` 2) ((tmpH - h') `div` 2) w' h'
        in I.crop rect tmp :: I.RGB

getAdminAlbumRemoveR :: AlbumId -> Handler Html
getAdminAlbumRemoveR albumId = do
    redirectIfNotConnected

    runDB $ do
        pics <- selectList [PictureAlbum ==. albumId] []
        mapM_ (pictureRemove . entityKey) pics
        delete albumId

    redirect AdminAlbumsR

getAdminPictureRemoveR :: PictureId -> Handler Html
getAdminPictureRemoveR picId = do
    redirectIfNotConnected

    albumId <- runDB $ do
        pic <- get404 picId
        pictureRemove picId
        return $ pictureAlbum pic

    redirect (AdminAlbumR albumId)

pictureRemove :: PictureId -> YesodDB App ()
pictureRemove picId = do
    app <- getYesod

    delete picId
    liftIO $ do
        removeFile (picturePath   app picId)
        removeFile (miniaturePath app picId)

pictureForm :: Form (Maybe Text, FileInfo)
pictureForm html = flip renderDivs html $ (,)
        <$> aopt textField "Description (facultatif) : " Nothing
        <*> areq fileField "Fichier"                     Nothing
