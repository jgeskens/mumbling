module Handler.Mumbling where

import Import
import Yesod.Form.Bootstrap3 ( BootstrapFormLayout (..)
                             , renderBootstrap3
                             , bfs
                             )
import Yesod.Auth.BrowserId (createOnClick)
import qualified Data.Text as T

mumbleForm :: Text -> Form (Maybe Text)
mumbleForm domain = renderBootstrap3 BootstrapBasicForm $ Just
  <$> areq textField (bfs ("How are you feeling about " ++ domain ++ "?" :: Text)) Nothing

emailToDomain :: Text -> Text
emailToDomain email
  | '@' `elem` email = drop 1 $ dropWhile (/= '@') email
  | otherwise = ""

extractDomain :: Entity User -> Text
extractDomain userEntity =
  let (Entity _ user) = userEntity
      email = userIdent user
  in emailToDomain email

slugify :: Text -> Text
slugify toSlugify = T.pack [if f c then c else '-' | c <- prepped]
  where f = (`elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']))
        prepped = T.unpack $ T.toLower toSlugify

getOrCreateOrganization :: Entity User -> Handler (Entity Organization)
getOrCreateOrganization userEntity = do
  let domain = extractDomain userEntity
  maybeOrg <- runDB $ getBy $ UniqueOrganizationDomain domain
  case maybeOrg of
    Just org -> return org
    Nothing -> do
      let orgObj = Organization domain
      orgId <- runDB $ insert orgObj
      return $ Entity orgId orgObj

getOrCreateMumbler :: Entity User -> Handler (Entity Mumbler)
getOrCreateMumbler userEntity = do
  let (Entity userId user) = userEntity
      email = userIdent user
  maybeMumbler <- runDB $ getBy $ UniqueMumblerEmail email
  (Entity orgId _) <- getOrCreateOrganization userEntity
  case maybeMumbler of
    Just mumbler -> return mumbler
    Nothing -> do
      let mumblerObj = Mumbler orgId (Just userId) email
      mumblerId <- runDB $ insert mumblerObj
      return $ Entity mumblerId mumblerObj

getMumblingR :: Handler Html
getMumblingR = do
  ma <- maybeAuth
  case ma of
    Nothing -> defaultLayout $ do
      loginFunction <- createOnClick def AuthR
      setTitle "Mumbling.net"
      $(widgetFile "mumbling")
    Just user -> do
      (formWidget, formEnctype) <- generateFormPost $ mumbleForm $ extractDomain user
      defaultLayout $ do
        setTitle "Mumbling.net"
        $(widgetFile "mumbling-form")

postMumblingR :: Handler Html
postMumblingR = do
  ma <- maybeAuth
  case ma of
    Nothing -> redirect $ AuthR LoginR
    Just user@(Entity maid u) -> do
      ((result, formWidget), formEnctype) <- runFormPost $ mumbleForm $ extractDomain user
      case result of
        FormSuccess (Just stmt) -> do
          let domain = extractDomain user
          maybeOrg <- runDB $ getBy $ UniqueOrganizationDomain domain
          org <- case maybeOrg of
            Just (Entity orgId _) -> return orgId
            Nothing -> runDB $ insert $ Organization domain
          maybeMumbler <- runDB $ getBy $ UniqueMumblerEmail $ userIdent u
          mumbler <- case maybeMumbler of
            Just (Entity mumblerId _) -> return mumblerId
            Nothing -> runDB $ insert $ Mumbler org (Just maid) $ userIdent u
          mumble <- runDB $ insert $ Mumble org stmt (slugify stmt) Nothing
          _ <- runDB $ insert $ MumbleVote mumble mumbler 1
          redirect OrganizationR
        _ -> defaultLayout $ do
          setTitle "Mumbling"
          $(widgetFile "mumbling-form")

widthRatio :: Int -> Int -> Int -> Int
widthRatio v m w =
  let vD = fromIntegral v
      mD = fromIntegral m
      wD = fromIntegral w
  in floor (vD * wD / mD :: Double) :: Int

mumbleStats :: Entity Mumble -> Handler (Entity Mumble, Int, Int, Int, Int)
mumbleStats mumble@(Entity mId _) = do
  agrees <- runDB $ count [MumbleVoteMumbleId ==. mId, MumbleVoteAnswer ==. 1]
  disagrees <- runDB $ count [MumbleVoteMumbleId ==. mId, MumbleVoteAnswer ==. -1]
  dontcares <- runDB $ count [MumbleVoteMumbleId ==. mId, MumbleVoteAnswer ==. 0]
  let total = agrees + disagrees + dontcares
      agrees' = widthRatio agrees total 100
      disagrees' = widthRatio disagrees total 100
      dontcares' = widthRatio dontcares total 100
  return (mumble, agrees', disagrees', dontcares', total)

getOrganizationR :: Handler Html
getOrganizationR = do
  ma <- maybeAuth
  case ma of
    Nothing -> redirect $ AuthR LoginR
    Just (Entity _ user) -> do
      let email = userIdent user
          domain = drop 1 $ dropWhile (/= '@') email
      redirect $ OrganizationPrettyR domain

getOrganizationPrettyR :: Text -> Handler Html
getOrganizationPrettyR domain = do
  maybeOrg <- runDB $ getBy $ UniqueOrganizationDomain domain
  (Entity orgId org) <- case maybeOrg of
    Just o -> return o
    Nothing -> do
      oId <- runDB $ insert $ Organization domain
      return $ Entity oId $ Organization domain

  mumbles <- runDB $ selectList [MumbleOrganizationId ==. orgId] [Asc MumbleId]

  -- Loop over the mumbles and add the count
  mumblesWithCount <- mapM mumbleStats mumbles

  defaultLayout $ do
    setTitle $ toHtml $ "Organization " ++ domain
    $(widgetFile "organization")

getMumbleR :: MumbleId -> Handler Html
getMumbleR mumbleId = do
  ma <- maybeAuth
  mToken <- fmap reqToken getRequest
  case ma of
    Nothing -> redirect $ AuthR LoginR
    Just u -> do
      let domain = extractDomain u
      mumble <- runDB $ get404 mumbleId
      Entity mumblerId mumbler <- getOrCreateMumbler u
      ownVotes <- runDB $ selectList [MumbleVoteMumblerId ==. mumblerId, MumbleVoteMumbleId ==. mumbleId] []

      -- Only show mumblings of the right organization
      when (mumbleOrganizationId mumble /= mumblerOrganizationId mumbler) $ redirect MumblingR

      stats <- mumbleStats $ Entity mumbleId mumble

      defaultLayout $ do
        setTitle $ toHtml $ mumbleStatement mumble ++ " at " ++ domain
        $(widgetFile "mumble")

getMumblePrettyR :: Text -> Text -> Handler Html
getMumblePrettyR domain mumbleSlug = do
  Entity orgId _ <- runDB $ getBy404 $ UniqueOrganizationDomain domain
  Entity mumbleId _ <- runDB $ getBy404 $ UniqueMumble mumbleSlug orgId
  getMumbleR mumbleId

postMumbleVoteR :: MumbleId -> Int -> Handler Html
postMumbleVoteR mumbleId answer = do
  ma <- maybeAuth
  case ma of
    Nothing -> redirect $ AuthR LoginR
    Just u -> do
      Entity orgId org <- getOrCreateOrganization u
      mumble <- runDB $ get404 mumbleId
      Entity mumblerId _ <- getOrCreateMumbler u
      ownVotes <- runDB $ selectList [MumbleVoteMumblerId ==. mumblerId, MumbleVoteMumbleId ==. mumbleId] []
      -- Only allow votes for the right organization
      when (mumbleOrganizationId mumble /= orgId) $ redirect MumblingR
      when (null ownVotes) $ do
        _ <- runDB $ insert $ MumbleVote mumbleId mumblerId answer
        return ()
      redirect $ MumblePrettyR (organizationDomain org) $ mumbleSlug mumble

composeEmail :: Text
composeEmail = error "not implemented"
