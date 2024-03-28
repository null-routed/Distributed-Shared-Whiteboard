package it.unipi.dsmt.jakartaee.app.ejb;

import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.dto.LoginInformationsDTO;
import it.unipi.dsmt.jakartaee.app.interfaces.UserEJB;
import jakarta.annotation.Nullable;
import jakarta.annotation.Resource;
import jakarta.ejb.Stateless;
import org.jetbrains.annotations.NotNull;

import javax.sql.DataSource;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;

/**
 * EJBs responsible for handling all the business logic related to the user management.
 */
@Stateless
public class UserEJBImplementation implements UserEJB {

    // Data source to MySQL database
    @Resource(lookup = "jdbc/SharedWhiteboardsPool")            // TODO: add connector to glassfish manager
    private DataSource dataSource;

    /**
     * Execute login procedure.
     * @param loginInformation object containing the user's login data.
     * @return a LoggedUserDTO object if login is successful, null otherwise
     */
    @Override
    public @Nullable LoggedUserDTO login(@NotNull LoginInformationsDTO loginInformation) {

        try (Connection connection = dataSource.getConnection()) {
            // Check if username and password are correct
            String query = String.format(
                "SELECT UserID " +
                "FROM %s WHERE `username` = ? AND `password` = ?"
            );

            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                // Set parameters in prepared statement
                preparedStatement.setString(1, loginInformation.getUsername());
                preparedStatement.setString(2, loginInformation.getPassword());

                // Execute query
                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    // If the query returned some results, then the login is successful!
                    if (resultSet.next()) {
                        return new LoggedUserDTO(
                                resultSet.getString("id"),
                                loginInformation.getUsername(),
                                loginInformation.getRole()
                        );
                    } else {
                        return null;
                    }
                }
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Function that search users based on a specified string
     * @param entered_string String on which the search is performed
     * @param role role of the users to search
     * @param index index that indicate current page to be shown
     * @return list of GeneralUserDTOs representing the users
     */
    public List<GeneralUserDTO> searchUsers(String entered_string, UserRole role, int index){
        //List to return
        List<GeneralUserDTO> users = new ArrayList<>();
        //Try connection
        try (Connection connection = dataSource.getConnection()) {
            String tableName =
                    (role == UserRole.student)
                            ? UserRole.student.name()
                            : UserRole.professor.name();
            String whereCondition =
                    (entered_string != null  && !entered_string.equals(""))
                            ? "WHERE `username` LIKE ?"
                            : "";
            // Build query
            String query = String.format(
                    "SELECT BIN_TO_UUID(`id`) as id, `username`, `email`, `name`, `surname` " +
                            "FROM %s %s ORDER BY username DESC LIMIT 10 OFFSET %d;",
                    tableName, whereCondition, index * 10
            );
            // Build prepared string
            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                // Look in the correct table
                if(entered_string != null && !entered_string.equals("")) {
                    preparedStatement.setString(1, "%" + entered_string + "%");
                }

                // Execute query
                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    // Build GeneralUserDTO for each result
                    while (resultSet.next()) {
                        GeneralUserDTO usr = new GeneralUserDTO();
                        usr.setId(resultSet.getString("id"));
                        usr.setUsername(resultSet.getString("username"));
                        usr.setEmail(resultSet.getString("email"));
                        usr.setName(resultSet.getString("name"));
                        usr.setSurname(resultSet.getString("surname"));
                        users.add(usr);
                    }
                    return users;
                }
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Function that remove the user account that the admin choose to remove
     * @param id userID to ban
     * @param role role of the user that have to be banned
     * @return True if the operation has been successful, false otherwise
     */
    public boolean banUser(String id, UserRole role) {
        //Try connection to datasource
        try (Connection connection = dataSource.getConnection()) {
            String tableName =
                    (role == UserRole.student)
                            ? UserRole.student.name()
                            : UserRole.professor.name();
            //build query
            String query = String.format("DELETE FROM %s WHERE `id` = UUID_TO_BIN(?);", tableName);

            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                // Look in the correct table
                preparedStatement.setString(1, id);

                // Execute query
                int ret = preparedStatement.executeUpdate();
                return ret == 1;
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * function that create a new professor account on the platform
     * @param createProfessorDTO is the dto with data of the professor
     * @return the result of the insert
     */
    @Override
    public boolean createProfessorAccount(@NotNull CreateProfessorDTO createProfessorDTO){
        try(Connection connection = dataSource.getConnection()) {
            String query = "INSERT INTO professor VALUES ( UUID_TO_BIN(UUID()) ,?, ?, ?, ?, ?);";

            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                // Set parameters in prepared statement
                preparedStatement.setString(1, createProfessorDTO.getUsername());
                preparedStatement.setString(2, createProfessorDTO.getPassword());
                preparedStatement.setString(3, createProfessorDTO.getEmail());
                preparedStatement.setString(4, createProfessorDTO.getName());
                preparedStatement.setString(5, createProfessorDTO.getSurname());

                // Execute query
                return preparedStatement.executeUpdate() == 1;
            }
        }
        catch (SQLException e) {
            return false;
        }
    }

    /**
     * definition of the signup operation. Takes a SignupDTO, perform the query and return the result.
     * @param signupDTO: SignupDTO type which contains user data to be stored
     * @return boolean value indicating if the signup operation was successful or not
     */
    @Override
    public boolean signup(@NotNull SignupDTO signupDTO){
        try(Connection connection = dataSource.getConnection()) {
            //Prepare the query
            String query = "INSERT INTO `student` VALUES ( UUID_TO_BIN(UUID()) ,?, ?, ?, ?, ? ,?, ?);";

            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                // Set parameters in prepared statement
                preparedStatement.setString(1, signupDTO.getUsername());
                preparedStatement.setString(2, signupDTO.getPassword());
                preparedStatement.setString(3, signupDTO.getEmail());
                preparedStatement.setString(4, signupDTO.getName());
                preparedStatement.setString(5, signupDTO.getSurname());
                preparedStatement.setString(6, signupDTO.getDegree());
                preparedStatement.setString(7, signupDTO.getLanguage());

                // Execute query
                int result = preparedStatement.executeUpdate();
                // evaluate the return value
                return result == 1;
            }
        }
        catch (SQLException e) {
            return false;
        }
    }
}
