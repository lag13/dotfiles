Mysql
=====

Mysql is a database.

Installing
----------

Mac: Already installed.

Staring/Stopping The Mysql Server
---------------------------------

It seems like it can be done with these commands:

```
mysql.server start
mysql.server stop
```

You can then connect by just going to `127.0.0.1:3306` as you might expect.

Which User Should I Use?
------------------------

It seems like the 'root' user has all the permissions needed. On my local
instance of mysql I do not need to enter a password. You can see the users on
a mysql server by doing:

```sql
select User FROM mysql.user;
```

Queries
-------

### Create Database + Table

Here's an example of creating a table plus the database to hold said table:

```
CREATE DATABASE tsrfeatureflags;

USE tsrfeatureflags;

CREATE TABLE featureflags
(
id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,
creationTime datetime DEFAULT CURRENT_TIMESTAMP,
modifiedTime TIMESTAMP NOT NULL DEFAULT '0000-00-00 00:00:00' ON UPDATE CURRENT_TIMESTAMP,
customerName VARCHAR(32) NOT NULL DEFAULT "",
flagName VARCHAR(64) NOT NULL,
flagOn CHARACTER(1) NOT NULL DEFAULT "0",
CONSTRAINT flag_uq UNIQUE (customerName, flagName)
);
```

### INSERT

```
INSERT INTO TABLE_NAME (column1, column2, column3,...columnN)]  
VALUES (value1, value2, value3,...valueN);
 
INSERT INTO TABLE_NAME VALUES (value1,value2,value3,...valueN);
```

### UPDATE

```
UPDATE table_name
SET column1=value1, column2=value2, ...
WHERE some_column=some_value;
```

### Insert Or Update

Sometimes you want to insert new data or update data if a row already exists.
There is a handy sql statement for that which I got from here:
http://stackoverflow.com/questions/4205181/insert-into-a-mysql-table-or-update-if-exists

```
INSERT INTO table_name (column1, column2, column3,...columnN)
VALUES(value1, value2, value3,...valueN)
ON DUPLICATE KEY UPDATE columnx = X, columny = Y, ...
```

Note that this updates a row if the row being inserted violates the UNIQUE
constraint of the table. I'm not sure on the specifics but you can define a
"unique constraint" on one or more columns. A primary key is an example of a
column which automatically has a unique constraint put on it. This means that
if a row with a primary key is inserted and that primary key already exists,
the insertion won't happen. Here are some examples of adding unique
constraints from: http://www.w3schools.com/sql/sql_unique.asp:

```
-- A single unique constraint
CREATE TABLE Persons
(
P_Id int NOT NULL,
LastName varchar(255) NOT NULL,
FirstName varchar(255),
Address varchar(255),
City varchar(255),
UNIQUE (P_Id)
)

-- A unique constraint involving multiple rows
CREATE TABLE Persons
(
P_Id int NOT NULL,
LastName varchar(255) NOT NULL,
FirstName varchar(255),
Address varchar(255),
City varchar(255),
CONSTRAINT constraint_name UNIQUE (P_Id,LastName)
)

ALTER TABLE Persons
ADD UNIQUE (P_Id)

ALTER TABLE Persons
ADD CONSTRAINT constraint_name UNIQUE (P_Id,LastName)
```

Modes
-----

This article is everything (and more) that I know):
http://dev.mysql.com/doc/refman/5.7/en/sql-mode.html

I guess the sql server can be in different modes which slightly alters how it
behaves. The only mode I really know about is strict (and its opposite
non-strict) modes this about sums up the little I know:

		If strict mode is not in effect, MySQL inserts adjusted values for invalid or
		missing values and produces warnings (see Section 14.7.5.40, “SHOW WARNINGS
		Syntax”). In strict mode, you can produce this behavior by using INSERT IGNORE
		or UPDATE IGNORE.

You can see the mode of a server like this:

```
show session variables where Variable_name = "sql_mode”;
```

Basically when the server is in "strict" mode anything "unexpected" will throw
an error. Such examples would be:

1. Inserting a string longer than the column width
2. Not inserting data into a NOT NULL column with no DEFAULT clause

But when in "non-strict" mode those sorts of things will just throw warnings.
You can see the warnings for the last query by running:

```
show warnings;
```

Ping Database With mysql-client
-------------------------------

If you just want to ping the database until it becomes active, you can do
something like this:

```
until mysql -u root -p$mysqlRootPassword  -e ";" ; do
	echo "error connecting to mysql database, retrying..."
done
```
