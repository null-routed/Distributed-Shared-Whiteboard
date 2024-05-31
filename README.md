![Mnesia](https://img.shields.io/badge/Mnesia-ED7B06?style=flat&logo=erlang&logoColor=white)
![Erlang](https://img.shields.io/badge/Erlang-A90533?style=flat&logo=erlang&logoColor=white)
![MySQL](https://img.shields.io/badge/MySQL-4479A1?style=flat&logo=mysql&logoColor=white)
![Java](https://img.shields.io/badge/Java-ED8B00?style=flat&logo=openjdk&logoColor=white)
![JavaScript](https://img.shields.io/badge/JavaScript-F7DF1E?style=flat&logo=javascript&logoColor=black)
![Bootstrap](https://img.shields.io/badge/Bootstrap-7952B3?style=flat&logo=bootstrap&logoColor=white)

## Starting and terminating the Glassfish 7.0.10 server:
* In `glassfish-7.0.10\glassfish7\bin` execute `./asadmin start-domain` to start `domain1`
* In `glassfish-7.0.10\glassfish7\bin` execute `./asadmin stop-domain` to stop `domain1`

<br>

## Opening and deploying this project using IntelliJ IDEA Ultimate Edition IDE and a local instance of Glassfish 7.0.10:
### 1. Download this repository and open `DSMT/src/app` with Intellij IDEA Ultimate edition
### 2. Delegating build and run actions to Intellij IDEA
* File -> Sttings -> Build, Execution, Deployment -> Build Tools -> Maven -> Runner
* Check the 'Delegate IDE build/run actions to Maven' checkbox
* Apply -> OK
### 3. Compiling the 'ejb' and 'ejb-interfaces' modules
* **If the 'Maven' button is not showing in the right sidebar:** Open the `pom.xml` file -> Right click anywhere in the file -> Click on 'Add as Maven project' (do this for each `pom.xml` file in the project)
* Click on 'Maven' in the right sidebar -> ejb -> Lifecycle -> Double-click on 'install'
* Click on 'Maven' in the right sidebar -> ejb-interfaces -> Lifecycle -> Double-click on 'compile'
### 4. Configuring the EJB application
* File -> Project Structure -> Artifacts
* `+` button -> EJB Application: Exploded
  * Name: ejb
  * Click on 'ejb' under 'Available Elements' -> Double-click on ''ejb' compile output'
  * Click on 'ejb-interfaces' under 'Available Elements' -> Double-click on ''ejb-interfaces' compile output'
  * Apply -> OK
### 5. Setting up the configuration to a local instance of Glassfish 7.0.10
* Current File -> Edit Configurations...
* Add new... -> Glassfish Server -> Local
* **Server Tab:**
  * Server Domain -> select `domain1`
  * Set Username and Password to your Glassfish Server credentials
  * Set JRE to `corretto-11 (Amazon Corretto version 11....)` 
* **Deployment Tab:** (**here the ordering of operations is very important**):
  * `+` button -> Artifact... -> select ejb -> OK
  * `+` button -> Artifact... -> select web:war exploded -> OK
* **Server Tab:**
  * Choose your preferred 'On update action' and 'On frame deactivation' actions
* Apply -> OK
### 6. Creating and setting up a Glassfish 7.0.10 JDBC Connection Pool
* Navigate and login to the Glassfish Admin Console (localhost:4848). The default credentials are `admin` and a blank password.
* JDBC -> JDBC Connection Pools -> New...
* **General Settings:**
  * **Pool Name:** `Shared Whiteboards Pool`
  * **Resource Type:** `javax.sql.DataSource`
  * **Database Driver Vendor:** `MySQL`
* Next
* **Datasource Classname** (if not already set): `com.mysql.jdbc.jdbc2.optional.MysqlDataSource`
* **Additional Properties** (manually add the following or update the entries if already showing):
  <br><br>
  | **Name**     | **Value** |
  |:---:|:---:|
  | `PortNumber`   | `3306`      |
  | `Port`         | `3306`      |
  | `UseSSL`       | `false`     |
  | `URL`          | `jdbc:mysql://localhost:3306/shared_whiteboards` |
  | `Url`          | `jdbc:mysql://localhost:3306/shared_whiteboards` |
  | `ServerName`   | `localhost` |
  | `DatabaseName` | `shared_whiteboards` |
  | `User`         | `\<your MySQL username>` |
  | `Password`     | `\<your MySQL password>` |
 * **Note:** if your MySQL instance login credentials have never been modified, those will be `root`, `root`.
* Finish
* **Note:** if you have your local MySQL instance up and running (e.g. through XAMPP) on port 3306, by clicking the 'Ping' button, the ping should be successful
### 7. Creating and setting up a Glassfish 7.0.10 JDBC Resource
* Navigate and login to the Glassfish Admin Console (localhost:4848). The default credentials are `admin` and a blank password.
* JDBC -> JDBC Resources -> New...
  * **JNDI Name:** `jdbc/SharedWhiteboardsPool`
  * **Pool Name:** `SharedWhiteboardsPool`
* OK
