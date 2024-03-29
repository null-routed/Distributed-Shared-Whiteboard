## Opening and deploying this project using IntelliJ IDEA Ultimate Edition IDE and a local instance of Glassfish 7.0.10:
### 1) Download this repo and open DSMT/src/app with Intellij IDEA Ultimate edition
### 2) Delegating build and run actions to Intellij IDEA
* File -> Sttings -> Build, Execution, Deployment -> Build Tools -> Maven -> Runner
* Check the 'Delegate IDE build/run actions to Maven' checkbox
### 3) Compiling the 'ejb' and 'ejb-interfaces' modules
* Click on 'Maven' in the right sidebar -> ejb -> Lifecycle -> Double-click on 'install'
* Click on 'Maven' in the right sidebar -> ejb-interfaces -> Lifecycle -> Double-click on 'compile'
### 4) Configuring the EJB application
* File -> Project Structure -> Artifacts
* '+' button -> EJB APplication: Exploded
  * Name: ejb
  * Click on 'ejb' under 'Available Elements' -> Double-click on ''ejb' compile output'
  * CLick on 'ejb-interfaces' under 'Available Elements' -> Double-click on ''ejb-interfaces' compile output'
  * Apply -> OK
### 5) Setting up the configuration to Glassfish 7.0.10
* Current File -> Edit Configurations...
* Add new... -> Glassfish Server -> Local
* Server Tab:
  * Server Domain -> select 'domain1'
  * Set Username and Password to your Glassfish Server credentials
  * Set JRE to 'corretto-11 (Amazon Corretto version 11....)' 
* Deployment Tab (here the ordering of operation is very important):
  * '+' button -> Artifact... -> select ejb -> OK
  * '+' button -> Artifact... -> select web:war exploded -> OK
* Server Tab:
  * Choose your preferred 'On update action' and 'On frame deactivation' actions
* Apply -> OK
