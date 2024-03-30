package it.unipi.dsmt.jakartaee.app.ejb;

import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.dto.LoginInformationsDTO;
import it.unipi.dsmt.jakartaee.app.dto.SignupDTO;
import it.unipi.dsmt.jakartaee.app.interfaces.UserEJB;
import it.unipi.dsmt.jakartaee.app.enums.SignupStatus;
import jakarta.annotation.Nullable;
import jakarta.annotation.Resource;
import jakarta.ejb.Stateless;
import org.jetbrains.annotations.NotNull;
import org.apache.commons.codec.digest.DigestUtils;

import javax.sql.DataSource;
import java.sql.*;


/**
 * EJBs responsible for handling all the business logic related to the user management.
 */
@Stateless
public class UserEJBImplementation implements UserEJB {

    // Data source to MySQL database
    @Resource(lookup = "jdbc/SharedWhiteboardsPool")
    private DataSource dataSource;

    /**
     * Execute login procedure.
     * @param loginInformation object containing the user's login data.
     * @return a LoggedUserDTO object if login is successful, null otherwise
     */
    @Override
    public @Nullable LoggedUserDTO login(@NotNull LoginInformationsDTO loginInformation) {

        System.out.println("UserEJBImplementation: called 'login' method...");

        try (Connection connection = dataSource.getConnection()) {
            // Check if username and password are correct
            String query = "SELECT UserID FROM Users WHERE username = ? AND password = ?";

            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                // Set parameters in prepared statement
                preparedStatement.setString(1, loginInformation.getUsername());
                preparedStatement.setString(2, loginInformation.getPassword());

                // Execute query
                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    // Query returned something, login successful
                    if (resultSet.next()) {
                        return new LoggedUserDTO(
                            resultSet.getString("UserID"),
                            loginInformation.getUsername()
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
     * definition of the signup operation. Takes a SignupDTO, perform the query and return the result.
     *
     * @param signupDTO: SignupDTO type which contains user data to be stored
     * @return SignupStatus value indicating if the signup operation was successful or not, if so error code
     */
    @Override
    public SignupStatus signup(@NotNull SignupDTO signupDTO){
        System.out.println("UserEJBImplementation: called 'signup' method...");

        try(Connection connection = dataSource.getConnection()) {
            // Query preparation
            String query = "INSERT INTO Users VALUES (?, ?, ?, ?, ?);";

            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                // Set parameters in prepared statement
                preparedStatement.setString(1, signupDTO.getUsername());

                // hashing password before memorizing in DB -> SHA-256
                String hashedPassword = DigestUtils.sha256Hex(signupDTO.getPassword());
                preparedStatement.setString(2, hashedPassword);

                preparedStatement.setString(3, signupDTO.getName());
                preparedStatement.setString(4, signupDTO.getSurname());
                preparedStatement.setString(5, signupDTO.getEmail());

                // Execute query
                int result = preparedStatement.executeUpdate();

                // evaluate the return value
                if (result == 1) {
                    return SignupStatus.SUCCESS;
                } else
                    return SignupStatus.OTHER_ERROR;            // general error
            }
        } catch (SQLException e) {
            // duplicate entry on 'Username' or 'Email' columns
            if (e.getSQLState().equals("23000") && e.getErrorCode() == 1062) {      // general violation of uniqueness error
                String SQLErrorMsg =  e.getMessage().toLowerCase();
                if(SQLErrorMsg.contains("username"))
                    return SignupStatus.DUPLICATE_USERNAME;
                if (SQLErrorMsg.contains("email"))
                    return SignupStatus.DUPLICATE_EMAIL;
            }

            return SignupStatus.OTHER_ERROR;        // general error
        }
    }
}
