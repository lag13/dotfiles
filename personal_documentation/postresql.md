https://www.postgresql.org/docs/9.4/libpq-pgpass.html

for t in acls apis basicauth_credentials cluster_events consumers hmacauth_credentials jwt_secrets keyauth_credentials oauth2_authorization_codes oauth2_credentials oauth2_tokens plugins ratelimiting_metrics response_ratelimiting_metrics schema_migrations ssl_certificates ssl_servers_names targets ttls upstreams; do PGPASSFILE=.pgpass psql --host kong-prod.cpjrey4w4qff.us-east-1.rds.amazonaws.com --echo-queries kong kong -c "select * from $t" >> results.txt; done

psql --host db-server db-name username

## database interactions
\list - list dbs

select datname from pg_database;


\connect dbname

## table interactions
\d - list tables


\? - output all backslack commands
