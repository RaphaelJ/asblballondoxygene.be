module Foundation where

import Prelude
import Yesod
import Yesod.Static
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Settings.StaticFiles
import Database.Persist.Sql (SqlPersistT)
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import Text.Printf
import Yesod.Core.Types (Logger)
import Data.Int
import Data.Text (pack)
import Vision.Primitive
import System.FilePath ((</>))

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        (120 * 60) -- 120 minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
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
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    maximumContentLength _ (Just (AdminAlbumR _ )) = Nothing
    maximumContentLength _ _                       = Just $ 2 * 1024 * 1024

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Pictures Utilities ----------------------------------------------------------

miniatureSize :: DIM2
miniatureSize = Z :. 150 :. 200

picturesDir :: FilePath
picturesDir = "albums"

pictureNum :: PictureId -> Int64
pictureNum ~(Key (PersistInt64 picId)) = picId

pictureName, miniatureName :: PictureId -> FilePath
pictureName   picId = printf "%s.jpg"      (show $ pictureNum picId)
miniatureName picId = printf "%s_mini.jpg" (show $ pictureNum picId)

pictureRoute, miniatureRoute :: PictureId -> Route App
pictureRoute picId = 
    StaticR $ StaticRoute [pack picturesDir, pack (pictureName picId)] []

miniatureRoute picId =
    StaticR $ StaticRoute [pack picturesDir, pack (miniatureName picId)] []

picturePath, miniaturePath :: PictureId -> FilePath
picturePath   picId = Settings.staticDir </> picturesDir </> pictureName   picId
miniaturePath picId = Settings.staticDir </> picturesDir </> miniatureName picId
