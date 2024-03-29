{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Scaffold.Statement.File
  ( save,
    getMeta,
    Scaffold.Statement.File.delete,
    getHashWithBucket,
    patch,
  )
where

import Control.Foldl
import Control.Lens
import Data.Coerce
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Hasql.Statement as HS
import Hasql.TH
import Scaffold.Transport.Id
import Scaffold.Transport.Model.File

save :: HS.Statement [(Hash, Name, Mime, T.Text)] [Id "file"]
save =
  lmap
    ( V.unzip4
        . V.fromList
        . Prelude.map
          ( \x ->
              x
                & _1 %~ coerce @_ @T.Text
                & _2 %~ coerce @_ @T.Text
                & _3 %~ coerce @_ @T.Text
          )
    )
    $ statement
    $ premap (^. coerced) list
  where
    statement =
      [foldStatement|
        insert into storage.file
        (hash, title, mime, bucket)
        select x.hash, x.title, x.mime, x.bucket
        from unnest(
          $1 :: text[],
          $2 :: text[],
          $3 :: text[],
          $4 :: text[]) as
          x(hash, title, mime, bucket)
        returning id :: int8|]

getMeta :: HS.Statement (Id "file") (Maybe (Hash, Name, Mime, Bucket))
getMeta =
  dimap (^. coerced) (fmap mkTpl) $
    [maybeStatement|
    select hash :: text, title :: text, mime :: text, bucket :: text
    from storage.file
    where id = $1 :: int8
          and not is_deleted|]
  where
    mkTpl x = x & _1 %~ coerce & _2 %~ coerce & _3 %~ coerce & _4 %~ coerce

delete :: HS.Statement (Id "file") Bool
delete =
  dimap coerce (> 0) $
    [rowsAffectedStatement|
    update storage.file
    set deleted = now(),
    is_deleted = true
    where id = $1 :: int8 and not is_deleted :: bool|]

getHashWithBucket :: HS.Statement (Id "file") (Maybe (Hash, Bucket))
getHashWithBucket =
  dimap (^. coerced) (fmap (\x -> x & _1 %~ coerce & _2 %~ coerce)) $
    [maybeStatement|
    select hash :: text, bucket :: text
    from storage.file
    where id = $1 :: int8|]

patch :: HS.Statement (Name, Mime, Hash) ()
patch =
  lmap (\x -> x & _1 %~ coerce & _2 %~ coerce & _3 %~ coerce) $
    [resultlessStatement|
    update storage.file set
    title = $1 :: text,
    mime = $2 :: text,
    modified = now()
    where hash = $3 :: text|]
