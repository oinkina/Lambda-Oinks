-------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid            ((<>))
import           Hakyll
import qualified Data.Map as M
import           Text.Pandoc.Options
import           Data.Maybe (fromMaybe, isJust)
import           Control.Monad (filterM)
import qualified Data.Char as Char

import           Config as Config
-------------------------------------------------------------------------------
-----RULES-----

main :: IO ()
main = hakyllWith config $ do

    -- Compress CSS
    match ("css/*" 
            .||. "bootstrap/css/*" 
            .||. "highlight/styles/*"
            .||. "fonts/Serif/cmun-serif.css"
            .||. "fonts/Serif Slanted/cmun-serif-slanted.css") $ do
        route   idRoute
        compile compressCssCompiler

    -- Static files
    match ("js/*" 
            .||. "favicon.ico"
            .||. "bootstrap/js/*" 
            .||. "bootstrap/fonts/*" 
            .||. "images/*"
            .||. "images/highlight/*" 
            .||. "highlight/highlight.pack.js"
            .||. "fonts/Serif/*"
            .||. "fonts/Serif-Slanted/*"
            .||. "comments/*"
            .||. "js/MathBox.js/**"
            .||. "posts/**" .&&. (complement postPattern)) $ do
        route idRoute
        compile copyFileCompiler

    -- Move favicon to root
    match "images/favicon.ico" $ do
        route $ constRoute "favicon.ico" 
        compile copyFileCompiler

    -- Compile templates
    match "templates/*" $ compile templateCompiler

    -- Build tags field
    tags <- buildTags postPattern $ fromCapture "blog/tags/*"

    -- Use tags field for postCtx
    let taggedCtx = postCtx tags

    -- Compile home, about, contact
    match "pages/*.md" $ do
        route   $ gsubRoute "pages/" (const "") 
                    `composeRoutes` setExtension "html"
        compile $ myPandoc
              >>= loadAndApplyTemplate "templates/default.html" siteCtx
              >>= relativizeUrls

    -- Compile blog posts
    match postPattern $ do
        route $ gsubRoute "posts/" (const "blog/") 
                    `composeRoutes` setExtension "html"
        compile $ myPandoc
              >>= saveSnapshot "content"
              >>= return . fmap demoteHeaders -- h1 -> h2; h2 -> h3; etc
              >>= loadAndApplyTemplate "templates/post.html"    taggedCtx
              >>= loadAndApplyTemplate "templates/default.html" taggedCtx
              >>= relativizeUrls

    -- Create blog index page (archive)
    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< onlyPublished =<< loadAll postPattern

            let blogCtx =
                    listField "posts" taggedCtx (return posts)
                 <> constField "title" "Blog"
                 <> siteCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" blogCtx
                >>= loadAndApplyTemplate "templates/default.html" blogCtx
                >>= relativizeUrls

    -- Compile home page
    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< onlyPublished =<< loadAll postPattern
            
            let indexCtx =
                    listField "posts" taggedCtx (return posts)
                 <> constField "title" "Home"
                 <> siteCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/home.html" indexCtx
                >>= loadAndApplyTemplate "templates/index_template.html" indexCtx
                >>= relativizeUrls

    -- Create tag pages
    tagsRules tags $ \tag pattern -> do
        route   $ gsubRoute " " (const "_") `composeRoutes` setExtension ".html"
        compile $ makeListPage tags pattern (capitalized tag ++ " Posts")

        version "rss" $ do
            route   $ setExtension "xml"
            compile $ makeRssFeed tags pattern

    -- Create RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ makeRssFeed tags postPattern

-------------------------------------------------------------------------------
----- CONTEXTS ------

siteCtx :: Context String
siteCtx = defaultContext
       <> mathCtx
       <> constField "blogName" Config.name

postCtx :: Tags -> Context String
postCtx tags = dateField "date" "%B %e, %Y"
            <> tagsField "tags" tags
            <> urlstripCtx
            <> siteCtx

-- MathJax
mathCtx :: Context String
mathCtx = field "mathjax" $ \item -> do
    metadata <- getMetadata $ itemIdentifier item
    return $ "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
    
-- Gets rid of "/index.html" from posts
urlstripCtx :: Context a
urlstripCtx = field "url" $ \item -> do
    route <- getRoute (itemIdentifier item)
    return $ fromMaybe "/" $ 
        fmap (reverse . drop 10 . reverse) route

--------------------------------------------------------------------------------
----- HELPER FUNCTIONS ------

-- For filtering lists of items to only be published items
onlyPublished :: MonadMetadata m => [Item a] -> m [Item a]
onlyPublished = filterM isPublished where
    isPublished item = do
        pubfield <- getMetadataField (itemIdentifier item) "published"
        return (isJust pubfield)

-- Creates a list of posts with given tags, pattern, filter.
postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String]) -> Compiler String
postList tags pattern sortFilter = do
    posts        <- sortFilter =<< loadAll pattern
    itemTemplate <- loadBody "templates/postlink.html"
    applyTemplateList itemTemplate (postCtx tags) posts

-- Creates a page with a list of posts in it. 
-- We use this for tagged posts pages. 
makeListPage :: Tags -> Pattern -> String -> Compiler (Item String)
makeListPage tags pattern title = do
    let listCtx = field "postlist"   (\_ -> postList tags pattern postFilter)
               <> constField "title" title
               <> siteCtx

    makeItem ""
        >>= loadAndApplyTemplate "templates/postlist.html" listCtx
        >>= loadAndApplyTemplate "templates/default.html" listCtx
        >>= relativizeUrls

-- Create an RSS feed for a list of posts.
makeRssFeed :: Tags -> Pattern -> Compiler (Item String)
makeRssFeed tags pattern = do
    let feedCtx = postCtx tags <> bodyField "description"
    loadAllSnapshots pattern "content"
        >>= fmap (take 10) . postFilter
        >>= renderRss feedConfig feedCtx

-- Capitalization for tags
capitalizedWord :: String -> String
capitalizedWord (head:tail) = Char.toUpper head : map Char.toLower tail
capitalizedWord [] = []

capitalized :: String -> String
capitalized = unwords . map capitalizedWord . words

--------------------------------------------------------------------------------
----- CONFIGS ------

-- Deploy blog with: ./site deploy --
config = defaultConfiguration { deployCommand = Config.deploy }

myWriterOptions :: WriterOptions
myWriterOptions = defaultHakyllWriterOptions {
                      writerReferenceLinks = True
                    , writerHtml5 = True
                    , writerHighlight = True
                    , writerHTMLMathMethod = MathJax "http://cdn.mathjax.org/mathjax/latest/MathJax.js"
                    }

myPandoc = pandocCompilerWith defaultHakyllReaderOptions myWriterOptions

-- RSS feed -- 
feedConfig :: FeedConfiguration
feedConfig = FeedConfiguration
    { feedTitle       = Config.name
    , feedDescription = Config.description
    , feedAuthorName  = Config.author
    , feedAuthorEmail = Config.email
    , feedRoot        = Config.url
    }

postPattern = "posts/*/index.md"
postFilter x = recentFirst =<< onlyPublished x