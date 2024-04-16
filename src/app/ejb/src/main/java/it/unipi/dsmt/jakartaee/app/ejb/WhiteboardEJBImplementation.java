package it.unipi.dsmt.jakartaee.app.ejb;

import it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO;
import it.unipi.dsmt.jakartaee.app.dto.WhiteboardCreationDTO;
import it.unipi.dsmt.jakartaee.app.enums.ParticipantOperationStatus;
import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import jakarta.annotation.Resource;
import jakarta.ejb.Stateless;
import jakarta.validation.constraints.NotNull;

import javax.imageio.ImageIO;
import javax.sql.DataSource;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
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
                    "SELECT W.WhiteboardID, W.Name, W.Description, W.WhiteboardSnapshot " +
                    "FROM Whiteboards W " +
                        "INNER JOIN WhiteboardParticipants WP " +
                            "ON W.WhiteboardID = WP.WhiteboardID " +
                    "WHERE W.Name LIKE ?;";

            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                preparedStatement.setString(1, "%" + whiteboardName + "%");

                // Executing query
                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    while (resultSet.next())
                        whiteboards.add(
                                new MinimalWhiteboardDTO(
                                        resultSet.getInt("W.WhiteboardID"),
                                        resultSet.getString("W.Name"),
                                        resultSet.getString("W.Description"),
                                        resultSet.getBytes("W.WhiteboardSnapshot")
                                )
                        );
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

        final String query =
                "SELECT W.WhiteboardID, W.Name, W.Description, W.WhiteboardSnapshot " +
                "FROM Whiteboards W " +
                    "INNER JOIN WhiteboardParticipants WP " +
                        "ON W.WhiteboardID = WP.WhiteboardID " +
                "WHERE WP.UserID = ?";


        try (Connection connection = dataSource.getConnection()) {

            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {

                preparedStatement.setString(1, userId);

                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    while (resultSet.next())
                        whiteboards.add(new MinimalWhiteboardDTO(
                                        resultSet.getInt("W.WhiteboardID"),
                                        resultSet.getString("W.Name"),
                                        resultSet.getString("W.Description"),
                                        resultSet.getBytes("W.WhiteboardSnapshot")
                                )
                        );
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
        System.out.println("@WhiteboardEJBImplementation: called getsSharedWhiteboards() method");

        List<MinimalWhiteboardDTO> whiteboards = new ArrayList<>();

        // SQL query
        final String query =
                "SELECT W.WhiteboardID, W.Name, W.Description, W.WhiteboardSnapshot " +
                "FROM Whiteboards W " +
                    "INNER JOIN WhiteboardParticipants WP " +
                        "ON W.WhiteboardID = WP.WhiteboardID " +
                "WHERE WP.UserID = ? AND WP.IsOwner = FALSE";

        try (Connection connection = dataSource.getConnection()) {
            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                preparedStatement.setString(1, id);

                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    while (resultSet.next())
                        whiteboards.add(
                                new MinimalWhiteboardDTO(
                                    resultSet.getInt("W.WhiteboardID"),
                                    resultSet.getString("W.Name"),
                                    resultSet.getString("W.Description"),
                                    resultSet.getBytes("W.WhiteboardSnapshot")
                                )
                        );
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
                    while (resultSet.next())
                        participantUsernames.add(resultSet.getString("Username"));

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

        final String insertWhiteboardQuery = "INSERT INTO Whiteboards(Name, Description, ReadOnly) VALUES (?, ?, ?)";
        final String insertParticipantQuery = "INSERT INTO WhiteboardParticipants(WhiteboardID, UserID, IsOwner) VALUES (?, ?, ?)";

        try (Connection connection = dataSource.getConnection()) {

            try (PreparedStatement whiteboardStatement = connection.prepareStatement(insertWhiteboardQuery, Statement.RETURN_GENERATED_KEYS);
                 PreparedStatement participantStatement = connection.prepareStatement(insertParticipantQuery)) {

                whiteboardStatement.setString(1, newWhiteboard.getName());
                whiteboardStatement.setString(2, newWhiteboard.getDescription());
                whiteboardStatement.setBoolean(3, newWhiteboard.isReadOnly());

                int rowsAffected = whiteboardStatement.executeUpdate();
                if (rowsAffected == 0)
                    return -1; // Return -1 to indicate failure

                // Retrieve the generated WhiteboardID
                try (ResultSet generatedKeys = whiteboardStatement.getGeneratedKeys()) {
                    if (generatedKeys.next()) {
                        int whiteboardId = generatedKeys.getInt(1);

                        // Set parameters for whiteboardparticipants table
                        participantStatement.setInt(1, whiteboardId);
                        participantStatement.setString(2, ownerId);
                        participantStatement.setBoolean(3, true); // Assuming owner is always the owner

                        rowsAffected = participantStatement.executeUpdate();
                        if (rowsAffected == 0)
                            return -1; // Return -1 to indicate failure

                        return whiteboardId; // Return the generated whiteboardId
                    } else
                        return -1; // Return -1 to indicate failure
                }
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public MinimalWhiteboardDTO getWhiteboardByID (int whiteboardID) {
        System.out.println("@WhiteboardEJBImplementation: called getWhiteboardByID() method");

        try (Connection connection = dataSource.getConnection()) {
            final String query =
                    "SELECT W.Name AS Name, W.Description AS Description, U.Username AS Owner, W.ReadOnly AS isReadOnly " +
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
                    if (resultSet.next())
                        return new MinimalWhiteboardDTO(
                                whiteboardID,
                                resultSet.getString("Name"),        // Object gets built by 4-param constructor
                                resultSet.getString("Description"),
                                resultSet.getString("Owner"),
                                resultSet.getBoolean("isReadOnly")
                        );
                    else
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

        final String deleteParticipantQuery = "DELETE FROM WhiteboardParticipants WHERE WhiteboardID = ?";
        final String deleteWhiteboardQuery = "DELETE FROM Whiteboards WHERE WhiteboardID = ?";

        try (Connection connection = dataSource.getConnection()) {

            try (PreparedStatement participantStatement = connection.prepareStatement(deleteParticipantQuery);
                 PreparedStatement whiteboardStatement = connection.prepareStatement(deleteWhiteboardQuery)) {

                participantStatement.setString(1, whiteboardID);

                int rowsAffected = participantStatement.executeUpdate();
                if (rowsAffected == 0)
                    return false;

                whiteboardStatement.setString(1, whiteboardID);

                rowsAffected = whiteboardStatement.executeUpdate();
                return rowsAffected != 0;
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public boolean isWhiteboardOwner(String userId, String whiteboardId) {
        System.out.println("@WhiteboardEJBImplementation: called isWhiteboardOwner() method");

        try (Connection connection = dataSource.getConnection()) {

            final String query = "SELECT IsOwner FROM WhiteboardParticipants WHERE UserID = ? AND WhiteboardID = ?";
            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {
                preparedStatement.setString(1, userId);
                preparedStatement.setString(2, whiteboardId);

                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    // Check if there is a result
                    if (resultSet.next()) {
                        // Retrieve the value of IsOwner column
                        return resultSet.getBoolean("IsOwner");
                    } else      // No result found, user is not the owner
                        return false;
                }
            }
        } catch (SQLException e) {
            return false;
        }
    }

    @Override
    public ParticipantOperationStatus removeParticipant(String userId, String whiteboardId) {
        System.out.println("@WhiteboardEJBImplementation: called removeParticipant() method");

        try (Connection connection = dataSource.getConnection()) {

            final String query = "DELETE FROM WhiteboardParticipants WHERE UserID = ? AND WhiteboardID = ?";
            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {

                preparedStatement.setString(1, userId);
                preparedStatement.setString(2, whiteboardId);

                int rowsAffected = preparedStatement.executeUpdate();

                if (rowsAffected > 0)          // Entry successfully deleted
                    return ParticipantOperationStatus.SQL_SUCCESS;
                else                    // No rows were affected, entry may not exist
                    return ParticipantOperationStatus.UNREGISTERED_USER;
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public ParticipantOperationStatus isParticipant(String username, String whiteboardId) {
        System.out.println("@WhiteboardEJBImplementation: called isParticipant() method, params=" + username + ", " + whiteboardId);

        String userIDToBeChecked = "";

        // getting the ID of the user to which the username belongs
        try (Connection connection = dataSource.getConnection()) {

            final String query = "SELECT UserID FROM Users WHERE Username = ?";
            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {

                preparedStatement.setString(1, username);

                try (ResultSet resultSet = preparedStatement.executeQuery()) {

                    if (resultSet.next())
                        userIDToBeChecked = resultSet.getString("UserID");
                }
            }
        } catch (SQLException e) {
            return ParticipantOperationStatus.OTHER_ERROR;            // an exception occurred
        }

        try (Connection connection = dataSource.getConnection()) {

            final String query = "SELECT COUNT(*) AS Counter FROM WhiteboardParticipants WHERE UserID = ? AND WhiteboardID = ?";
            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {

                preparedStatement.setString(1, userIDToBeChecked);
                preparedStatement.setString(2, whiteboardId);

                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    if (resultSet.next()) {
                        if (resultSet.getInt("Counter") == 1)
                            return ParticipantOperationStatus.ALREADY_PARTICIPATING;   // Entry exists, user is participating
                        else
                            return ParticipantOperationStatus.NOT_PARTICIPATING;
                    }
                }
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }

        return ParticipantOperationStatus.OTHER_ERROR;        // Default to error
    }

    @Override
    public ParticipantOperationStatus addParticipant(String username, String whiteboardId) {
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
                        return ParticipantOperationStatus.UNREGISTERED_USER;      // the provided username is not present in the DB
                }
            }
        } catch (SQLException e) {
            return ParticipantOperationStatus.OTHER_ERROR;            // an exception occurred
        }

        try (Connection connection = dataSource.getConnection()) {

            final String query = "INSERT INTO WhiteboardParticipants(WhiteboardID, UserID, IsOwner) VALUES (?, ?, ?)";

            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {

                preparedStatement.setString(1, whiteboardId);
                preparedStatement.setString(2, userIDToBeAdded);
                preparedStatement.setBoolean(3, false);     // method called by the owner, he cannot add himself to his whiteboard again

                int rowsAffected = preparedStatement.executeUpdate();

                if (rowsAffected > 0)
                   return ParticipantOperationStatus.SQL_SUCCESS;             // all good
            }
        } catch (SQLException e) {
            return ParticipantOperationStatus.OTHER_ERROR;            // an exception occurred
        }

        return ParticipantOperationStatus.OTHER_ERROR;        // Default to error
    }

    @Override
    public boolean updateWhiteboardSnapshot (byte[] snapshotDataBytes, String whiteboardID) {
        //System.out.println("@WhiteboardEJBImplementation: called updateWhiteboardSnapshot() method");

        try (Connection connection = dataSource.getConnection()) {

            final String query =
                    "UPDATE Whiteboards SET WhiteboardSnapshot = ? " +
                    "WHERE WhiteboardID = ?";

            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {

                preparedStatement.setBytes(1, snapshotDataBytes);
                preparedStatement.setString(2, whiteboardID);

                int affectedRows = preparedStatement.executeUpdate();

                return affectedRows > 0;
            }
        } catch (SQLException e) {
            return false;
        }
    }

    /*@Override
    public byte[] getSnapshotByWhiteboardID (String whiteboardID, String userID) {
        System.out.println("@WhiteboardEJBImplementation: called getSnapshotByWhiteboardID()");

        byte[] snapshot = null;

        try (Connection connection = dataSource.getConnection()) {

            final String query = "SELECT WhiteboardSnapshot FROM WhiteboardParticipants WHERE WhiteboardID = ? AND UserID = ?";

            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {

                preparedStatement.setString(1, whiteboardID);
                preparedStatement.setString(2, userID);

                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    if (resultSet.next()) {
                        snapshot = resultSet.getBytes("WhiteboardSnapshot");
                        if (snapshot != null)
                            return snapshot;
                        else {      // Snapshot is NULL, return a SNAPSHOT_WIDTH * SNAPSHOT_HEIGHT px blank image to use as snapshot
//                            System.out.println("RETURNING WHITE SNAPSHOT FOR WHITEBOARD " + whiteboardID);
                            final int SNAPSHOT_WIDTH = 300;
                            final int SNAPSHOT_HEIGHT = 160;
                            BufferedImage blankSnapshot = new BufferedImage(SNAPSHOT_WIDTH, SNAPSHOT_HEIGHT, BufferedImage.TYPE_INT_RGB);
                            Graphics2D graphics2D = blankSnapshot.createGraphics();
                            graphics2D.setColor(Color.WHITE);
                            graphics2D.fillRect(0, 0, SNAPSHOT_WIDTH, SNAPSHOT_HEIGHT);
                            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
                            ImageIO.write(blankSnapshot, "jpg", outputStream);
                            return outputStream.toByteArray();
                        }
                    } else {
                        throw new SQLException();
                    }
                }
            }
        } catch (SQLException |IOException e) {
            return null;
        }
    }*/
    @Override
    public byte[] getSnapshotByWhiteboardID (String whiteboardID) {
        //System.out.println("@WhiteboardEJBImplementation: called getSnapshotByWhiteboardID()");

        byte[] snapshot = null;

        try (Connection connection = dataSource.getConnection()) {

            final String query = "SELECT WhiteboardSnapshot FROM Whiteboards WHERE WhiteboardID = ?";

            try (PreparedStatement preparedStatement = connection.prepareStatement(query)) {

                preparedStatement.setString(1, whiteboardID);

                try (ResultSet resultSet = preparedStatement.executeQuery()) {
                    if (resultSet.next()) {
                        snapshot = resultSet.getBytes("WhiteboardSnapshot");
                        if (snapshot != null)
                            return snapshot;
                        else {
                            throw new SQLException();
                        }
                    } else {
                        throw new SQLException();
                    }
                }
            }
        } catch (SQLException e) {
            return null;
        }
    }
}
