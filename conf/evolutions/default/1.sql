# --- !Ups

create table "user" ("userID" VARCHAR NOT NULL PRIMARY KEY,"firstName" VARCHAR,"lastName" VARCHAR,"fullName" VARCHAR,"email" VARCHAR,"avatarURL" VARCHAR);
create table "logininfo" ("id" BIGINT GENERATED BY DEFAULT AS IDENTITY(START WITH 1) NOT NULL PRIMARY KEY,"providerID" VARCHAR NOT NULL,"providerKey" VARCHAR NOT NULL);
create table "userlogininfo" ("userID" VARCHAR NOT NULL,"loginInfoId" BIGINT NOT NULL);
create table "passwordinfo" ("hasher" VARCHAR NOT NULL,"password" VARCHAR NOT NULL,"salt" VARCHAR,"loginInfoId" BIGINT NOT NULL);
create table "oauth1info" ("id" BIGINT GENERATED BY DEFAULT AS IDENTITY(START WITH 1) NOT NULL PRIMARY KEY,"token" VARCHAR NOT NULL,"secret" VARCHAR NOT NULL,"loginInfoId" BIGINT NOT NULL);
create table "oauth2info" ("id" BIGINT GENERATED BY DEFAULT AS IDENTITY(START WITH 1) NOT NULL PRIMARY KEY,"accesstoken" VARCHAR NOT NULL,"tokentype" VARCHAR,"expiresin" INTEGER,"refreshtoken" VARCHAR,"logininfoid" BIGINT NOT NULL);
create table "openidinfo" ("id" VARCHAR NOT NULL PRIMARY KEY,"logininfoid" BIGINT NOT NULL);
create table "openidattributes" ("id" VARCHAR NOT NULL,"key" VARCHAR NOT NULL,"value" VARCHAR NOT NULL);
create table "userrole" ("userID" VARCHAR NOT NULL,"role" VARCHAR NOT NULL);
create table "adviser" ("id" BIGINT GENERATED BY DEFAULT AS IDENTITY(START WITH 1) NOT NULL PRIMARY KEY,"stuID" VARCHAR,"teaID" VARCHAR);
create table "detail" ("userID" VARCHAR NOT NULL,"topic" VARCHAR NOT NULL,"detail" VARCHAR NOT NULL,"objective" VARCHAR NOT NULL,"scope" VARCHAR NOT NULL,"technology" VARCHAR NOT NULL,"benefits" VARCHAR NOT NULL,"img" VARCHAR NOT NULL,"tea"  VARCHAR NOT NULL);
create table "commentProject" ("id" BIGINT GENERATED BY DEFAULT AS IDENTITY(START WITH 1) NOT NULL PRIMARY KEY,"detail" VARCHAR NOT NULL,"userID" VARCHAR NOT NULL,"projectID" VARCHAR NOT NULL);
# --- !Downs

drop table "openidattributes";
drop table "openidinfo";
drop table "oauth2info";
drop table "oauth1info";
drop table "passwordinfo";
drop table "userlogininfo";
drop table "logininfo";
drop table "user";
drop table "userrole";
drop table "adviser";
drop table "detail";
drop table "commentProject";
