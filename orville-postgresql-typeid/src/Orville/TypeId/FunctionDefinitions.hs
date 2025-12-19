{-# LANGUAGE QuasiQuotes #-}

{- | Contains Orville FunctionDefinitions that need to be loaded in order to support
generating a default value for a 'KindID' field definition

Code in this file is modified and derived from https://github.com/jetify-com/typeid-sql, licensed under Apache 2.0
-}
module Orville.TypeId.FunctionDefinitions
  ( uuidGenerateV7
  , base32Encode
  , base32Decode
  , typeIdGenerateText
  , typeIdPrint
  ) where

import qualified Orville.PostgreSQL as O
import qualified Orville.PostgreSQL.Expr as Expr
import qualified Orville.PostgreSQL.Raw.RawSql as RawSql
import qualified Orville.PostgreSQL.Schema as Schema
import Text.RawString.QQ (r)

{- | Function to generate new v7 UUIDs.

From https://github.com/jetify-com/typeid-sql/blob/main/sql/01_uuidv7.sql

@since 0.1.0.0
-}
uuidGenerateV7 :: O.FunctionDefinition
uuidGenerateV7 =
  O.mkFunction
    "uuid_generate_v7"
    []
    Expr.returnTypeUUID
    Schema.BeforeTableMigration
    Expr.plpgsql
    [r|
    declare
      unix_ts_ms bytea;
      uuid_bytes bytea;
    begin
      unix_ts_ms = substring(int8send(floor(extract(epoch from clock_timestamp()) * 1000)::bigint) from 3);
      uuid_bytes = uuid_send(gen_random_uuid());
      uuid_bytes = overlay(uuid_bytes placing unix_ts_ms from 1 for 6);
      uuid_bytes = set_byte(uuid_bytes, 6, (b'0111' || get_byte(uuid_bytes, 6)::bit(4))::bit(8)::int);
      return encode(uuid_bytes, 'hex')::uuid;
    end
    |]

{- | Function to encode a UUID as a base32 string

From https://github.com/jetify-com/typeid-sql/blob/main/sql/02_base32.sql

@since 0.1.0.0
-}
base32Encode :: O.FunctionDefinition
base32Encode =
  O.mkFunction
    "base32_encode"
    [RawSql.unsafeFromRawSql $ RawSql.fromString "id uuid"]
    Expr.returnTypeText
    Schema.BeforeTableMigration
    Expr.plpgsql
    [r|
    declare
      bytes bytea;
      alphabet bytea = '0123456789abcdefghjkmnpqrstvwxyz';
      output text = '';
    begin
      bytes = uuid_send(id);

      -- 10 byte timestamp
      output = output || chr(get_byte(alphabet, (get_byte(bytes, 0) & 224) >> 5));
      output = output || chr(get_byte(alphabet, (get_byte(bytes, 0) & 31)));
      output = output || chr(get_byte(alphabet, (get_byte(bytes, 1) & 248) >> 3));
      output = output || chr(get_byte(alphabet, ((get_byte(bytes, 1) & 7) << 2) | ((get_byte(bytes, 2) & 192) >> 6)));
      output = output || chr(get_byte(alphabet, (get_byte(bytes, 2) & 62) >> 1));
      output = output || chr(get_byte(alphabet, ((get_byte(bytes, 2) & 1) << 4) | ((get_byte(bytes, 3) & 240) >> 4)));
      output = output || chr(get_byte(alphabet, ((get_byte(bytes, 3) & 15) << 1) | ((get_byte(bytes, 4) & 128) >> 7)));
      output = output || chr(get_byte(alphabet, (get_byte(bytes, 4) & 124) >> 2));
      output = output || chr(get_byte(alphabet, ((get_byte(bytes, 4) & 3) << 3) | ((get_byte(bytes, 5) & 224) >> 5)));
      output = output || chr(get_byte(alphabet, (get_byte(bytes, 5) & 31)));

      -- 16 bytes of entropy
      output = output || chr(get_byte(alphabet, (get_byte(bytes, 6) & 248) >> 3));
      output = output || chr(get_byte(alphabet, ((get_byte(bytes, 6) & 7) << 2) | ((get_byte(bytes, 7) & 192) >> 6)));
      output = output || chr(get_byte(alphabet, (get_byte(bytes, 7) & 62) >> 1));
      output = output || chr(get_byte(alphabet, ((get_byte(bytes, 7) & 1) << 4) | ((get_byte(bytes, 8) & 240) >> 4)));
      output = output || chr(get_byte(alphabet, ((get_byte(bytes, 8) & 15) << 1) | ((get_byte(bytes, 9) & 128) >> 7)));
      output = output || chr(get_byte(alphabet, (get_byte(bytes, 9) & 124) >> 2));
      output = output || chr(get_byte(alphabet, ((get_byte(bytes, 9) & 3) << 3) | ((get_byte(bytes, 10) & 224) >> 5)));
      output = output || chr(get_byte(alphabet, (get_byte(bytes, 10) & 31)));
      output = output || chr(get_byte(alphabet, (get_byte(bytes, 11) & 248) >> 3));
      output = output || chr(get_byte(alphabet, ((get_byte(bytes, 11) & 7) << 2) | ((get_byte(bytes, 12) & 192) >> 6)));
      output = output || chr(get_byte(alphabet, (get_byte(bytes, 12) & 62) >> 1));
      output = output || chr(get_byte(alphabet, ((get_byte(bytes, 12) & 1) << 4) | ((get_byte(bytes, 13) & 240) >> 4)));
      output = output || chr(get_byte(alphabet, ((get_byte(bytes, 13) & 15) << 1) | ((get_byte(bytes, 14) & 128) >> 7)));
      output = output || chr(get_byte(alphabet, (get_byte(bytes, 14) & 124) >> 2));
      output = output || chr(get_byte(alphabet, ((get_byte(bytes, 14) & 3) << 3) | ((get_byte(bytes, 15) & 224) >> 5)));
      output = output || chr(get_byte(alphabet, (get_byte(bytes, 15) & 31)));

      return output;
    end
    |]

{- | Function to decode a base32 string as a UUID

From https://github.com/jetify-com/typeid-sql/blob/main/sql/02_base32.sql

@since 0.1.0.0
-}
base32Decode :: O.FunctionDefinition
base32Decode =
  O.mkFunction
    "base32_decode"
    [RawSql.unsafeFromRawSql $ RawSql.fromString "s text"]
    Expr.returnTypeUUID
    Schema.BeforeTableMigration
    Expr.plpgsql
    [r|
    declare
      dec bytea = '\xFF FF FF FF FF FF FF FF FF FF'::bytea ||
                  '\xFF FF FF FF FF FF FF FF FF FF'::bytea ||
                  '\xFF FF FF FF FF FF FF FF FF FF'::bytea ||
                  '\xFF FF FF FF FF FF FF FF FF FF'::bytea ||
                  '\xFF FF FF FF FF FF FF FF 00 01'::bytea ||
                  '\x02 03 04 05 06 07 08 09 FF FF'::bytea ||
                  '\xFF FF FF FF FF FF FF FF FF FF'::bytea ||
                  '\xFF FF FF FF FF FF FF FF FF FF'::bytea ||
                  '\xFF FF FF FF FF FF FF FF FF FF'::bytea ||
                  '\xFF FF FF FF FF FF FF 0A 0B 0C'::bytea ||
                  '\x0D 0E 0F 10 11 FF 12 13 FF 14'::bytea ||
                  '\x15 FF 16 17 18 19 1A FF 1B 1C'::bytea ||
                  '\x1D 1E 1F FF FF FF FF FF FF FF'::bytea ||
                  '\xFF FF FF FF FF FF FF FF FF FF'::bytea ||
                  '\xFF FF FF FF FF FF FF FF FF FF'::bytea ||
                  '\xFF FF FF FF FF FF FF FF FF FF'::bytea ||
                  '\xFF FF FF FF FF FF FF FF FF FF'::bytea ||
                  '\xFF FF FF FF FF FF FF FF FF FF'::bytea ||
                  '\xFF FF FF FF FF FF FF FF FF FF'::bytea ||
                  '\xFF FF FF FF FF FF FF FF FF FF'::bytea ||
                  '\xFF FF FF FF FF FF FF FF FF FF'::bytea ||
                  '\xFF FF FF FF FF FF FF FF FF FF'::bytea ||
                  '\xFF FF FF FF FF FF FF FF FF FF'::bytea ||
                  '\xFF FF FF FF FF FF FF FF FF FF'::bytea ||
                  '\xFF FF FF FF FF FF FF FF FF FF'::bytea ||
                  '\xFF FF FF FF FF FF'::bytea;
      v bytea = convert_to(s, 'UTF8');
      id bytea = '\x00000000000000000000000000000000';
    begin
      if length(s) <> 26 then
        raise exception 'typeid suffix must be 26 characters';
      end if;

      if get_byte(dec, get_byte(v, 0)) = 255 or
        get_byte(dec, get_byte(v, 1)) = 255 or
        get_byte(dec, get_byte(v, 2)) = 255 or
        get_byte(dec, get_byte(v, 3)) = 255 or
        get_byte(dec, get_byte(v, 4)) = 255 or
        get_byte(dec, get_byte(v, 5)) = 255 or
        get_byte(dec, get_byte(v, 6)) = 255 or
        get_byte(dec, get_byte(v, 7)) = 255 or
        get_byte(dec, get_byte(v, 8)) = 255 or
        get_byte(dec, get_byte(v, 9)) = 255 or
        get_byte(dec, get_byte(v, 10)) = 255 or
        get_byte(dec, get_byte(v, 11)) = 255 or
        get_byte(dec, get_byte(v, 12)) = 255 or
        get_byte(dec, get_byte(v, 13)) = 255 or
        get_byte(dec, get_byte(v, 14)) = 255 or
        get_byte(dec, get_byte(v, 15)) = 255 or
        get_byte(dec, get_byte(v, 16)) = 255 or
        get_byte(dec, get_byte(v, 17)) = 255 or
        get_byte(dec, get_byte(v, 18)) = 255 or
        get_byte(dec, get_byte(v, 19)) = 255 or
        get_byte(dec, get_byte(v, 20)) = 255 or
        get_byte(dec, get_byte(v, 21)) = 255 or
        get_byte(dec, get_byte(v, 22)) = 255 or
        get_byte(dec, get_byte(v, 23)) = 255 or
        get_byte(dec, get_byte(v, 24)) = 255 or
        get_byte(dec, get_byte(v, 25)) = 255
      then
        raise exception 'typeid suffix must only use characters from the base32 alphabet';
      end if;

      if chr(get_byte(v, 0)) > '7' then
        raise exception 'typeid suffix must start with 0-7';
      end if;
      -- Transform base32 to binary array
      -- 6 bytes timestamp (48 bits)
      id = set_byte(id, 0, (get_byte(dec, get_byte(v, 0)) << 5) | get_byte(dec, get_byte(v, 1)));
      id = set_byte(id, 1, (get_byte(dec, get_byte(v, 2)) << 3) | (get_byte(dec, get_byte(v, 3)) >> 2));
      id = set_byte(id, 2, ((get_byte(dec, get_byte(v, 3)) & 3) << 6) | (get_byte(dec, get_byte(v, 4)) << 1) | (get_byte(dec, get_byte(v, 5)) >> 4));
      id = set_byte(id, 3, ((get_byte(dec, get_byte(v, 5)) & 15) << 4) | (get_byte(dec, get_byte(v, 6)) >> 1));
      id = set_byte(id, 4, ((get_byte(dec, get_byte(v, 6)) & 1) << 7) | (get_byte(dec, get_byte(v, 7)) << 2) | (get_byte(dec, get_byte(v, 8)) >> 3));
      id = set_byte(id, 5, ((get_byte(dec, get_byte(v, 8)) & 7) << 5) | get_byte(dec, get_byte(v, 9)));

      -- 10 bytes of entropy (80 bits)
      id = set_byte(id, 6, (get_byte(dec, get_byte(v, 10)) << 3) | (get_byte(dec, get_byte(v, 11)) >> 2));
      id = set_byte(id, 7, ((get_byte(dec, get_byte(v, 11)) & 3) << 6) | (get_byte(dec, get_byte(v, 12)) << 1) | (get_byte(dec, get_byte(v, 13)) >> 4));
      id = set_byte(id, 8, ((get_byte(dec, get_byte(v, 13)) & 15) << 4) | (get_byte(dec, get_byte(v, 14)) >> 1));
      id = set_byte(id, 9, ((get_byte(dec, get_byte(v, 14)) & 1) << 7) | (get_byte(dec, get_byte(v, 15)) << 2) | (get_byte(dec, get_byte(v, 16)) >> 3));
      id = set_byte(id, 10, ((get_byte(dec, get_byte(v, 16)) & 7) << 5) | get_byte(dec, get_byte(v, 17)));
      id = set_byte(id, 11, (get_byte(dec, get_byte(v, 18)) << 3) | (get_byte(dec, get_byte(v, 19)) >> 2));
      id = set_byte(id, 12, ((get_byte(dec, get_byte(v, 19)) & 3) << 6) | (get_byte(dec, get_byte(v, 20)) << 1) | (get_byte(dec, get_byte(v, 21)) >> 4));
      id = set_byte(id, 13, ((get_byte(dec, get_byte(v, 21)) & 15) << 4) | (get_byte(dec, get_byte(v, 22)) >> 1));
      id = set_byte(id, 14, ((get_byte(dec, get_byte(v, 22)) & 1) << 7) | (get_byte(dec, get_byte(v, 23)) << 2) | (get_byte(dec, get_byte(v, 24)) >> 3));
      id = set_byte(id, 15, ((get_byte(dec, get_byte(v, 24)) & 7) << 5) | get_byte(dec, get_byte(v, 25)));
      return encode(id, 'hex')::uuid;
    end
    |]

{- | Function to generate typeid as a text

Modified from https://github.com/jetify-com/typeid-sql/blob/main/sql/03_typeid.sql

Since we aren't defining the 'typeid' type in postgres, we modify 'typeid_print' to take two arguments

@since 0.1.0.0
-}
typeIdGenerateText :: O.FunctionDefinition
typeIdGenerateText =
  O.mkFunction
    "typeid_generate_text"
    [RawSql.unsafeFromRawSql $ RawSql.fromString "prefix text"]
    Expr.returnTypeText
    Schema.BeforeTableMigration
    Expr.plpgsql
    [r|
    begin
      if (prefix is null) or not (prefix ~ '^([a-z]([a-z_]{0,61}[a-z])?)?$') then
        raise exception 'typeid prefix must match the regular expression ^([a-z]([a-z_]{0,61}[a-z])?)?$';
      end if;
      return typeid_print(prefix, uuid_generate_v7());
    end
    |]

{- | Function to print a typeid as a text

Modified from https://github.com/jetify-com/typeid-sql/blob/main/sql/03_typeid.sql

Since we aren't defining the 'typeid' type in postgres, we modify 'typeid_print' to take two arguments

@since 0.1.0.0
-}
typeIdPrint :: O.FunctionDefinition
typeIdPrint =
  O.mkFunction
    "typeid_print"
    [RawSql.unsafeFromRawSql $ RawSql.fromString "tidtype text", RawSql.unsafeFromRawSql $ RawSql.fromString "tiduuid uuid"]
    Expr.returnTypeText
    Schema.BeforeTableMigration
    Expr.plpgsql
    [r|
    declare
      prefix text;
      suffix text;
    begin
      if (tidtype is null or tiduuid is null) then
        return null;
      end if;
      prefix = tidtype;
      suffix = base32_encode(tiduuid);
      if (prefix is null) or not (prefix ~ '^([a-z]([a-z_]{0,61}[a-z])?)?$') then
        raise exception 'typeid prefix must match the regular expression ^([a-z]([a-z_]{0,61}[a-z])?)?$';
      end if;
      if prefix = '' then
        return suffix;
      end if;
      return (prefix || '_' || suffix);
    end
    |]
