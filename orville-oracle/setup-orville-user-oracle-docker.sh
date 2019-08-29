#!/bin/sh

SQL="CREATE USER orville IDENTIFIED BY test;
GRANT CONNECT TO orville;
GRANT CREATE SESSION to orville;
GRANT CREATE ANY TABLE to orville;
GRANT UNLIMITED TABLESPACE TO orville;
quit;
"

FILE_CMD="echo \"$SQL\" >> /home/oracle/setup_user.sql"

SQL_PLUS_CMD='sqlplus sys/Oradoc_db1@ORCLPDB1 as sysdba @/home/oracle/setup_user.sql'

BASHRC_CMD='source /home/oracle/.bashrc'

docker-compose exec oracle bash -c "$BASHRC_CMD && $FILE_CMD && $SQL_PLUS_CMD "
