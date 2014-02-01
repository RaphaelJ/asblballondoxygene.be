module Handler.Albums where

import Import

getAlbumR :: AlbumId -> Handler Html
getAlbumR albumId = do
    (album, pics) <- runDB $ do
        album <- get404 albumId
        pics  <- selectList [PictureAlbum ==. albumId] [Asc PictureId]
        return (album, pics)

    let pics' = zip (cycle [1, 2, 3, 4 :: Int]) pics
    defaultLayout $ do
        setTitle [shamlet|#{albumName album} - ASBL Ballon d'Oxygène|]
        $(widgetFile "album")
