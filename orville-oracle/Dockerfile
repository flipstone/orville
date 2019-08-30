FROM flipstone/stack:v2-1.9.3

RUN apt-get update &&\
    apt-get install -y --no-install-recommends libaio1 unixodbc unixodbc-dev unzip &&\
    apt-get clean

COPY ./instantclient-basic-linux.x64-12.1.0.2.0.zip ./instantclient-basic-linux.x64-12.1.0.2.0.zip

COPY ./instantclient-odbc-linux.x64-12.1.0.2.0.zip ./instantclient-odbc-linux.x64-12.1.0.2.0.zip

COPY ./tnsnames.ora /tnsnames.ora

RUN unzip instantclient-odbc-linux.x64-12.1.0.2.0.zip -d /opt/oracle &&\
    unzip instantclient-basic-linux.x64-12.1.0.2.0.zip -d /opt/oracle &&\
    sh /opt/oracle/instantclient_12_1/odbc_update_ini.sh / /opt/oracle/instantclient_12_1
