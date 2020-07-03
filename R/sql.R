library(RSQLite)

conn <- dbConnect(SQLite(),'mycars.db')

dbGetQuery(conn, 'DROP TABLE IF EXISTS mer')

dbWriteTable(conn, "mer", mer)

q = dbGetQuery(conn,
               'SELECT m1.*
                FROM mer m1 JOIN mer m2
                WHERE m1.author = m2.author
                    AND m1.customer_review = m2.customer_review
                    AND m1.airline != m2.airline')
View(q)
