library(RPostgres)
p_user='mbuoro'
p_password <- "dajzoz$gokpu1%ciVsod"#key_get(service = "pggeodb.u3e.inrae.fr", username = p_user)
p_host = 'pggeodb.u3e.inrae.fr'
p_dbname="db_u3e"
p_port = '5432'
p_sslmode = "require"

db_con <- dbConnect(Postgres(),
                 user = p_user,
                 password = p_password,
                 dbname = p_dbname,
                 host = p_host,
                 port = p_port,
                 sslmode = p_sslmode
)



# list of tables:
#dbListTables(con)
#df <- dbGetQuery(con, statement = read_file('Nivelle/code/vcaptures_nivelle.sql'))

#dbDisconnect(db_con)


