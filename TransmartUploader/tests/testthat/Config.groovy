dataDir = 'ETL'

db.hostname = 'pyro'
db.port = 5432

db.sid = 'xe'
db.username = 'tm_dataloader'
db.password = 'tm_dataloader'

// Uncomment the following 3 lines for PostgreSQL:

db.jdbcConnectionString = "jdbc:postgresql://${db.hostname}:${db.port}/transmart"
db.jdbcDriver = 'org.postgresql.Driver'
db.sql.storedProcedureSyntax = 'PostgreSQL'

