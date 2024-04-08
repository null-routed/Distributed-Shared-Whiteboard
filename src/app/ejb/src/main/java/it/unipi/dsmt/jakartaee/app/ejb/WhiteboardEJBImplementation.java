package it.unipi.dsmt.jakartaee.app.ejb;

import it.unipi.dsmt.jakartaee.app.dto.WhiteboardCreationDTO;
import it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO;
import it.unipi.dsmt.jakartaee.app.enums.AddParticipantStatus;
import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import jakarta.annotation.Resource;
import jakarta.ejb.Stateless;
import jakarta.validation.constraints.NotNull;

import javax.sql.DataSource;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;


@Stateless
public class WhiteboardEJBImplementation implements WhiteboardEJB {

    // Data source to MySQL database
    @Resource(lookup = "jdbc/SharedWhiteboardsPool")
    private DataSource dataSource;

    @Override
    public List<MinimalWhiteboardDTO> searchWhiteboard(String whiteboardName) {
        System.out.println("@WhiteboardEJBImplementation: called searchWhiteboard() method");

        List<MinimalWhiteboardDTO> whiteboards = new ArrayList<>();

        try (Connection connection = dataSource.getConnection()) {
            final String query =
                    "SELECT W.WhiteboardID, W.Name, W.Description " +
                    "FROM Whiteboards W " +
                        "INNER JOIN WhiteboardParticipants WP " +
                            "ON W.WhiteboardID = WP.WhiteboardID " +
                    "WHERE W.Name LIKE ?;";

            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                preparedStatement.setString(1, "%" + whiteboardName + "%");

                // Executing query
                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    while (resultSet.next()){
                        whiteboards.add(
                                new MinimalWhiteboardDTO(
                                        resultSet.getInt("W.WhiteboardID"),
                                        resultSet.getString("W.Name"),
                                        resultSet.getString("W.Description")
                                )
                        );
                    }
                }
            }
        }
        catch (SQLException e) {
            throw new RuntimeException(e);
        }
        return whiteboards;
    }

    @Override
    public List<MinimalWhiteboardDTO> getAllWhiteboards(@NotNull String userId) {
        System.out.println("@WhiteboardEJBImplementation: called getAllWhiteboards() method");

        List<MinimalWhiteboardDTO> whiteboards = new ArrayList<>();

        // SQL queries
        final String query =
                "SELECT W.WhiteboardID, W.Name, W.Description " +
                "FROM Whiteboards W " +
                    "INNER JOIN WhiteboardParticipants WP " +
                        "ON W.WhiteboardID = WP.WhiteboardID " +
                "WHERE WP.UserID = ?";


        try (Connection connection = dataSource.getConnection()) {
            // Get details of requested course
            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                // Set the parameter (user ID)
                preparedStatement.setString(1, userId);
                // Execute query
                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    // If the query returned a result set,
                    // then wrap it inside a CourseDTO object and return it
                    while (resultSet.next()){
                        whiteboards.add(new MinimalWhiteboardDTO(
                                        resultSet.getInt("W.WhiteboardID"),
                                        resultSet.getString("W.Name"),
                                        resultSet.getString("W.Description")
                                )
                        );
                    }
                }
            }
        }
        catch (SQLException e) {
            throw new RuntimeException(e);
        }
        return whiteboards;
    }

    @Override
    public List<MinimalWhiteboardDTO> getSharedWhiteboards(String id) {
        List<MinimalWhiteboardDTO> whiteboards = new ArrayList<>();

        // SQL query
        final String query =
                "SELECT W.WhiteboardID, W.Name, W.Description " +
                "FROM Whiteboards W " +
                    "INNER JOIN Whiteboardparticipants WP " +
                        "ON W.WhiteboardID = WP.WhiteboardID " +
                "WHERE WP.UserID = ? AND WP.IsOwner = FALSE";

        try (Connection connection = dataSource.getConnection()) {
            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                preparedStatement.setString(1, id);

                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    while (resultSet.next()) {
                        whiteboards.add(
                                new MinimalWhiteboardDTO(
                                    resultSet.getInt("W.WhiteboardID"),
                                    resultSet.getString("W.Name"),
                                    resultSet.getString("W.Description")
                                )
                        );
                    }
                }
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
        return whiteboards;
    }

    public List<String> getParticipantUsernames(int whiteboardID) {
        System.out.println("@WhiteboardEJBImplementation: called getParticipants() method");

        List<String> participantUsernames = new ArrayList<>();

        try (Connection connection = dataSource.getConnection()) {
            final String query =
                    "SELECT U.Username AS Username " +
                    "FROM WhiteboardParticipants WP " +
                        "INNER JOIN Users U " +
                            "ON WP.UserID = U.UserID " +
                    "WHERE WP.WhiteboardID = ?;";

            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                preparedStatement.setInt(1, whiteboardID);

                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    while (resultSet.next()) {
                        participantUsernames.add(resultSet.getString("Username"));
                    }

                    if (participantUsernames.isEmpty())
                        participantUsernames = null;
                }
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
        return participantUsernames;
    }

    @Override
    public int addWhiteboard(String ownerId, WhiteboardCreationDTO newWhiteboard) {
        System.out.println("@WhiteboardEJBImplementation: called addWhiteboard() method");

        // SQL query to insert into the whiteboards table
        final String insertWhiteboardQuery = "INSERT INTO Whiteboards(Name, Description, ReadOnly) VALUES (?, ?, ?)";

        // SQL query to insert into the whiteboardparticipants table
        final String insertParticipantQuery = "INSERT INTO WhiteboardParticipants(WhiteboardID, UserID, IsOwner) VALUES (?, ?, ?)";

        try (Connection connection = dataSource.getConnection()) {
            // Start a transaction
            connection.setAutoCommit(false);

            try (PreparedStatement whiteboardStatement = connection.prepareStatement(insertWhiteboardQuery, Statement.RETURN_GENERATED_KEYS);
                 PreparedStatement participantStatement = connection.prepareStatement(insertParticipantQuery)) {

                // Set parameters for whiteboards table
                whiteboardStatement.setString(1, newWhiteboard.getName());
                whiteboardStatement.setString(2, newWhiteboard.getDescription());
                whiteboardStatement.setBoolean(3, newWhiteboard.isReadOnly());

                // Execute insert into whiteboards table
                int rowsAffected = whiteboardStatement.executeUpdate();
                if (rowsAffected == 0) {
                    // Rollback transaction if no rows were affected
                    connection.rollback();
                    return -1; // Return -1 to indicate failure
                }

                // Retrieve the generated WhiteboardID
                try (ResultSet generatedKeys = whiteboardStatement.getGeneratedKeys()) {
                    if (generatedKeys.next()) {
                        int whiteboardId = generatedKeys.getInt(1);

                        // Set parameters for whiteboardparticipants table
                        participantStatement.setInt(1, whiteboardId);
                        participantStatement.setString(2, ownerId);
                        participantStatement.setBoolean(3, true); // Assuming owner is always the owner

                        // Execute insert into whiteboard participants table
                        rowsAffected = participantStatement.executeUpdate();
                        if (rowsAffected == 0) {
                            // Rollback transaction if no rows were affected
                            connection.rollback();
                            return -1; // Return -1 to indicate failure
                        }

                        // Commit transaction if all operations were successful
                        connection.commit();
                        return whiteboardId; // Return the generated whiteboardId
                    } else {
                        // Rollback transaction if no keys were generated
                        connection.rollback();
                        return -1; // Return -1 to indicate failure
                    }
                }
            }
        } catch (SQLException e) {
            // Handle SQL exception
            throw new RuntimeException(e);
        }
    }

    @Override
    public MinimalWhiteboardDTO getWhiteboardByID (int whiteboardID) {
        System.out.println("@WhiteboardEJBImplementation: called getWhiteboardByID() method");

        try (Connection connection = dataSource.getConnection()) {
            final String query =
                    "SELECT W.Name AS Name, W.Description AS Description, U.Username AS Owner " +
                    "FROM Whiteboards W " +
                        "INNER JOIN WhiteboardParticipants WP " +
                            "ON W.WhiteboardID = WP.WhiteboardID " +
                        "INNER JOIN Users U " +
                            "ON WP.UserID = U.UserID " +
                    "WHERE W.WhiteboardID = ? " +
                            "AND WP.IsOwner = 1";

            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                preparedStatement.setInt(1, whiteboardID);

                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    if (resultSet.next()) {
                        return new MinimalWhiteboardDTO(
                                whiteboardID,
                                resultSet.getString("Name"),        // Object gets built by 4-param constructor
                                resultSet.getString("Description"),
                                resultSet.getString("Owner")
                        );
                    } else
                        return null;
                }
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public boolean deleteWhiteboard(String whiteboardID) {
        System.out.println("@WhiteboardEJBImplementation: called deleteWhiteboard() method");

        // SQL query to delete from the WhiteboardParticipants table
        final String deleteParticipantQuery = "DELETE FROM WhiteboardParticipants WHERE WhiteboardID = ?";

        // SQL query to delete from the whiteboards table
        final String deleteWhiteboardQuery = "DELETE FROM Whiteboards WHERE WhiteboardID = ?";

        try (Connection connection = dataSource.getConnection()) {
            // Start a transaction
            connection.setAutoCommit(false);

            try (PreparedStatement participantStatement = connection.prepareStatement(deleteParticipantQuery);
                 PreparedStatement whiteboardStatement = connection.prepareStatement(deleteWhiteboardQuery)) {

                // Set parameter for whiteboardparticipants table
                participantStatement.setString(1, whiteboardID);

                // Execute delete from whiteboardparticipants table
                int rowsAffected = participantStatement.executeUpdate();
                if (rowsAffected == 0) {
                    // Rollback transaction if no rows were affected
                    connection.rollback();
                    return false;
                }

                // Set parameter for whiteboards table
                whiteboardStatement.setString(1, whiteboardID);

                // Execute delete from whiteboards table
                rowsAffected = whiteboardStatement.executeUpdate();
                if (rowsAffected == 0) {
                    // Rollback transaction if no rows were affected
                    connection.rollback();
                    return false;
                }

                // Commit transaction if all operations were successful
                connection.commit();
                return true;
            }
        } catch (SQLException e) {
            // Handle SQL exception
            throw new RuntimeException(e);
        }
    }

    @Override
    public boolean isWhiteboardOwner(String userId, String whiteboardId) {
        System.out.println("@WhiteboardEJBImplementation: called isWhiteboardOwner() method");
        try (Connection connection = dataSource.getConnection()) {
            // Prepare the SQL query
            String query = "SELECT IsOwner FROM WhiteboardParticipants WHERE UserID = ? AND WhiteboardID = ?";
            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                // Set the parameters
                preparedStatement.setString(1, userId);
                preparedStatement.setString(2, whiteboardId);

                // Execute the query
                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    // Check if there is a result
                    if (resultSet.next()) {
                        // Retrieve the value of IsOwner column
                        return resultSet.getBoolean("IsOwner");
                    } else {
                        // No result found, user is not the owner
                        return false;
                    }
                }
            }
        } catch (SQLException e) {
            // Handle SQLException
            e.printStackTrace();
            return false;
        }
    }

    @Override
    public boolean removeParticipant(String userId, String whiteboardId) {
        System.out.println("@WhiteboardEJBImplementation: called removeParticipant() method");
        try (Connection connection = dataSource.getConnection()) {
            // Prepare the SQL query to delete the entry
            final String query = "DELETE FROM WhiteboardParticipants WHERE UserID = ? AND WhiteboardID = ?";
            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                // Set the parameters
                preparedStatement.setString(1, userId);
                preparedStatement.setString(2, whiteboardId);

                // Execute the delete statement
                int rowsAffected = preparedStatement.executeUpdate();

                // Check if any rows were affected
                if (rowsAffected > 0) {
                    // Entry successfully deleted
                    return true;
                } else {
                    // No rows were affected, entry may not exist
                    return false;
                }
            }
        } catch (SQLException e) {
            // Handle SQLException
            throw new RuntimeException(e);
        }
    }

    @Override
    public AddParticipantStatus isParticipant(String username, String whiteboardId) {
        System.out.println("@WhiteboardEJBImplementation: called isParticipant() method, params=" + username + ", " + whiteboardId);

        String userIDToBeChecked = "";

        // getting the ID of the user to which the username belongs
        try (Connection connection = dataSource.getConnection()) {
            final String query = "SELECT UserID FROM Users WHERE Username = ?";
            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                preparedStatement.setString(1, username);
                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    if (resultSet.next()) {
                        userIDToBeChecked = resultSet.getString("UserID");
                    }
                }
            }
        } catch (SQLException e) {
            return AddParticipantStatus.OTHER_ERROR;            // an exception occurred
        }

        try (Connection connection = dataSource.getConnection()) {
            // Prepare the SQL query to count the matching entries
            final String query = "SELECT COUNT(*) AS Counter FROM WhiteboardParticipants WHERE UserID = ? AND WhiteboardID = ?";
            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                // Set the parameters
                preparedStatement.setString(1, userIDToBeChecked);
                preparedStatement.setString(2, whiteboardId);

                // Execute the query
                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    if (resultSet.next()) {
                        if (resultSet.getInt("Counter") == 1)
                            return AddParticipantStatus.ALREADY_PARTICIPATING;   // Entry exists, user is participating
                        else
                            return AddParticipantStatus.NOT_PARTICIPATING;
                    }
                }
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }

        return AddParticipantStatus.OTHER_ERROR;        // Default to error
    }

    @Override
    public AddParticipantStatus addParticipant(String username, String whiteboardId) {
        System.out.println("@WhiteboardEJBImplementation: called addParticipant() method, params=" + username + ", " + whiteboardId);

        String userIDToBeAdded = "";

        // getting the ID of the user to which the username belongs
        try (Connection connection = dataSource.getConnection()) {
            final String query = "SELECT UserID FROM Users WHERE Username = ?";
            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                preparedStatement.setString(1, username);
                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    if (resultSet.next())
                        userIDToBeAdded = resultSet.getString("UserID");
                    else
                        return AddParticipantStatus.UNREGISTERED_USER;      // the provided username is not present in the DB
                }
            }
        } catch (SQLException e) {
            return AddParticipantStatus.OTHER_ERROR;            // an exception occurred
        }

        try (Connection connection = dataSource.getConnection()) {
            // Prepare the SQL query to insert a new entry
            final String query = "INSERT INTO WhiteboardParticipants(WhiteboardID, UserID, IsOwner) VALUES (?, ?, ?)";
            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                // Set the parameters
                preparedStatement.setString(1, whiteboardId);
                preparedStatement.setString(2, userIDToBeAdded);
                preparedStatement.setBoolean(3, false);     // method called by the owner, he cannot add himself to his whiteboard again

                // Execute the insert statement
                int rowsAffected = preparedStatement.executeUpdate();

                // Check if any rows were affected
                if (rowsAffected > 0)
                   return AddParticipantStatus.SQL_SUCCESS;             // all good
            }
        } catch (SQLException e) {
            return AddParticipantStatus.OTHER_ERROR;            // an exception occurred
        }

        return AddParticipantStatus.OTHER_ERROR;        // Default to error
    }
}