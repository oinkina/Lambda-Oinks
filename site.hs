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
            .||. "extlibs/bootstrap/css/*" 
            .||. "extlibs/highlight/styles/*"
            .||. "fonts/Serif/cmun-serif.css"
            .||. "fonts/Serif Slanted/cmun-serif-slanted.css") $ do
        route   idRoute
        compile compressCssCompiler

    -- Static files
    match ("js/*" 
            .||. "extlibs/bootstrap/js/*" 
            .||. "extlibs/bootstrap/fonts/*" 
            .||. "extlibs/highlight/highlight.pack.js"
            .||. "extlibs/highlight/images/*"
            .||. "extlibs/mathbox/*"
            .||. "extlibs/mathbox/examples/*"
            .||. "blog/images/*" 
            .||. "fonts/Serif/*"
            .||. "fonts/Serif-Slanted/*"
            .||. "posts/**" .&&. (complement postPattern)) $ do
        route idRoute
        compile copyFileCompiler

    -- Move favicon to root
    match "blog/images/favicon.ico" $ do
        route $ constRoute "favicon.ico" 
        compile copyFileCompiler

    -- Compile templates
    match "templates/*" $ compile templateCompiler

    -- Build tags field
    tags <- buildTags postPattern $ fromCapture "blog/tags/*"

    -- Use tags field for postCtx
    let taggedCtx = postCtx tags

    -- Compile home, about, contact
    match "blog/pages/*.md" $ do
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

    -- Function to create home page and blog index page (archive)
    let createPage path title template1 template2 = create [path] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< onlyPublished =<< loadAll postPattern

            let pageCtx =
                    listField "posts" taggedCtx (return posts)
                 <> constField "title" title
                 <> siteCtx

            makeItem ""
                >>= loadAndApplyTemplate template1 pageCtx
                >>= loadAndApplyTemplate template2 pageCtx
                >>= relativizeUrls

    createPage "blog.html" "Blog" "templates/blog.html" "templates/default.html"
    createPage "index.html" "Home" "templates/home.html" "templates/index_template.html"

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

siteCtx :: Context String
siteCtx = defaultContext
       <> mathCtx
       <> constField "blogName" Config.name

postCtx :: Tags -> Context String
postCtx tags = dateField "date" "%B %e, %Y"
            <> tagsField "tags" tags
            <> urlstripCtx
            <> siteCtx

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

-- Creates a page with a list of posts in it; used for tagged posts pages. 
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