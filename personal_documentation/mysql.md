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

All About Databases
-------------------

### Where They Are Stored

Each databases is actually a directory somewhere in the file system. Where
specifically probably depends on how mysql was installed. I think you can
always find this information by reading the `my.cnf` file then looking for the
`datadir` key. For the 5.6 docker image of mysql for example
(`https://hub.docker.com/_/mysql/`) the databases are stored in
`/var/lib/mysql`.

Brifly looking at the contents of one of these database directories it appears
to be filled with files who's names are the names of the tables in the
database. All the tables seem to come as a pair of two files for example:

```
candidat.frm
candidat.ibd
```

It would appear that .idb stands for "InnoDB":
https://dev.mysql.com/doc/refman/5.7/en/innodb-multiple-tablespaces.html. It
would appear that the .frm stands for "format" (I'm guessing this file
describes the structure of the table while the .ibd file contains the data):
http://dev.mysql.com/doc/internals/en/frm-file-format.html.

### Error When Removing Them

One time when I was dropping a database I got this error:

```
ERROR 1010 (HY000) at line 1: Error dropping database (can't rmdir './test',
errno: 39)
```

This article helped when investigating:
http://stackoverflow.com/questions/12196996/mysql-error-dropping-database-errno-13-errno-17-errno-39.
In short that error will occur when there are files in the directory
representing that database which mysql does not feel like deleting for
whatever reason.

In my particular case it appeared that the only file that existed in that
directory was lien_objet_liste_119.ibd:

```
[root@e397d778a06d test]# ls -la
total 128
drwx------ 2 mysql mysql 28672 Jan  9 20:35 .
drwxr-x--- 6 mysql mysql  4096 Jan  9 20:12 ..
-rw-rw---- 1 mysql mysql 98304 Jan  9 18:42 lien_objet_liste_119.ibd
```

This is a flat out guess but I'm thinking that since there was not a pair of
tables lien_objet_liste_119.ibd and lien_objet_liste_119.frm mysql was
complaining. A total guess though. I'm not sure how this database got into
this state.


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

### ALTER

```
ALTER TABLE table_name
ADD col_name varchar(1000);

ALTER TABLE table_name
MODIFY col_name INT;

ALTER TABLE table_name
MODIFY col_name varchar(123);

ALTER TABLE table_name
DROP col_name;

ALTER TABLE `mytable`
ADD `accountDID` varchar(64) DEFAULT "",
ADD `productInstanceID` varchar(128) DEFAULT "",
ADD `category` varchar(32) DEFAULT "",
ADD INDEX `productInstanceID` (`productInstanceID`)
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

-- I don't know what the difference is between these two queries. I just don't
-- understand what exactly the CONSTRAINT piece does. It appears that both can be
-- used interchangeably.
ALTER TABLE Persons
ADD UNIQUE (P_Id)

ALTER TABLE Persons
ADD CONSTRAINT constraint_name UNIQUE (P_Id,LastName)

-- MYSQL drop unique constraint:
ALTER TABLE Persons
DROP INDEX uc_PersonID

-- Other sql's drop unique constraint:
ALTER TABLE Persons
DROP CONSTRAINT uc_PersonID
```

### DELETE

If you don't have the WHERE clause then everything will be deleted!!!!

```
DELETE FROM table_name
WHERE ...
```

### Declare Variable

Sometimes when writing a query it can be useful to declare a variable so it
can get used in a couple places. Here is an example of doing that:

```
SET @customer = 'sca';
select pkSite, strLibelle, strAdresse, strBDD, iSuppr,
case iArchi 
  when 0 then "dev"
  when 7 then "us"
  when 8 then "sg"
  when 9 then "eu"
end as datacenter,
iArchi
from site
where strLibelle like concat('%', @customer, '%')
OR strBDD = LEFT(@customer, 20);

```

### Delete Database

```
DROP DATABASE coolDatabaseName
```

### List Databases

```
SHOW DATABASES
```

### Subqueries

Here is an example of a subquery. Now, either I do not understand
subqueries well but this sort of thing is what makes sql confusing. I
select from some subquery which is basically a different table) but
I'm not actually selecting any columns from that table, I'm selecting
them from one of the joined tables.

select site.*
from (
select pkSite, count(*) as count
from site
where fkServeurBD = 52
and strBDD IN ("123points8","3a-academy","3a-academy-sandbox")
group by strBDD
) as subSite
inner join site on site.pkSite = subSite.pkSite
where subSite.count = 1
and site.iSuppr = 1;

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
show session variables where Variable_name = "sql_mode";
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

TODO
----

I guess there's a json datatype now:
http://dev.mysql.com/doc/refman/5.7/en/json.html
