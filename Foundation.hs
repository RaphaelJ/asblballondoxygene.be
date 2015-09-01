module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe

import Text.Printf
import Vision.Primitive

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static          -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool  -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        (120 * 60) -- 120 minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        mmsg   <- getMessage

        Just route <- getCurrentRoute

        albums <- runDB $ selectList [] [Asc AlbumName]

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"
            addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.0/css/bootstrap.min.css"
            addScriptRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.0/js/bootstrap.min.js"

            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    maximumContentLength _ (Just (AdminAlbumR _ )) = Nothing
    maximumContentLength _ _                       = Just $ 2 * 1024 * 1024

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Pictures Utilities ----------------------------------------------------------

miniatureSize :: DIM2
miniatureSize = Z :. 150 :. 200

picturesDir :: FilePath
picturesDir = "albums"

pictureNum :: PictureId -> Int64
pictureNum = fromIntegral . toBackendKey

pictureName, miniatureName :: PictureId -> FilePath
pictureName   picId = printf "%s.jpg"      (show $ pictureNum picId)
miniatureName picId = printf "%s_mini.jpg" (show $ pictureNum picId)

pictureRoute, miniatureRoute :: PictureId -> Route App
pictureRoute picId = 
    StaticR $ StaticRoute [pack picturesDir, pack (pictureName picId)] []

miniatureRoute picId =
    StaticR $ StaticRoute [pack picturesDir, pack (miniatureName picId)] []

picturePath, miniaturePath :: App -> PictureId -> FilePath
picturePath   app picId =
    appStaticDir (appSettings app) </> picturesDir </> pictureName   picId
miniaturePath app picId =
    appStaticDir (appSettings app) </> picturesDir </> miniatureName picId
