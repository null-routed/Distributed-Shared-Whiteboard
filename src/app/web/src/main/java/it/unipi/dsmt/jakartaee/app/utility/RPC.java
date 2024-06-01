package it.unipi.dsmt.jakartaee.app.utility;

import com.ericsson.otp.erlang.*;
import java.io.IOException;

/**
 * Static class providing methods to execute Remote Procedure Calls via Erlang
 */
public class RPC {

    private static OtpConnection connection = null;

    /**
     * Triggers the execution of an Erlang method depending on the command
     * @param command String describing what needs to be done
     * @param whiteboardId The whiteboard ID subject to the operation
     * @param userId The ID of the user triggering the operation
     * @param isOwner Tells if userId is the owner of the whiteboard identified by whiteboardId
     * @return a Boolean representing the outcome of the operation
     */
    public static boolean sendErlangWhiteboardUpdateRPC(String command, String whiteboardId, String userId, int isOwner) {
        System.out.println("@RPC: called sendErlangWhiteboardUpdateRPC() method");
        try {
            OtpErlangObject received = executeErlangCommand(command, whiteboardId, userId, isOwner);
            if (received instanceof OtpErlangAtom) {
                String result = ((OtpErlangAtom) received).atomValue();
                return "ok".equals(result);
            }
        } catch (Exception e) {
            System.out.println("@RPC: failed to execute remote call");
            return false;
        }
        return false;
    }


    /**
     * Establishes a connection to an Erlang node if not already connected.
     * @return OtpConnection instance representing the connection to the Erlang node.
     * @throws IOException if an I/O error occurs when attempting to establish the connection.
     */
    public static OtpConnection getConnection() throws IOException {
        if (connection == null || !connection.isConnected()) {
            OtpSelf self = new OtpSelf("java_client", "shared_whiteboard_app");
            OtpPeer peer = new OtpPeer("node1@localhost");
            try {
                connection = self.connect(peer);
                System.out.println("Java erlang node connection established.");
            } catch (Exception e) {
                System.out.println("@RPC: failed to get a connection. Cause:" + e.getCause());
                e.printStackTrace();
            }
        }
        return connection;
    }

    /**
     * Executes an Erlang command to handle whiteboard changes.
     * @param command the command to be executed
     * @param whiteboardId the ID of the whiteboard
     * @param userId the ID of the user
     * @param isOwner flag indicating if the user is the owner
     * @return OtpErlangObject representing the response from the Erlang node
     * @throws IOException if an I/O error occurs when communicating with the Erlang node
     * @throws OtpAuthException if authentication fails when connecting to the Erlang node
     * @throws OtpErlangExit if the Erlang process exits unexpectedly
     */
    public static OtpErlangObject executeErlangCommand(String command, String whiteboardId, String userId, int isOwner)
            throws IOException, OtpAuthException, OtpErlangExit {
        OtpConnection conn = getConnection();           // Connecting to Erlang node

        if (conn == null) {
            System.err.println("@RPC: called executeErlangCommand() method, failed to establish connection.");
            return null;
        }

        OtpErlangAtom arg1 = new OtpErlangAtom(command);

        byte[] whiteboardIdBytes = whiteboardId.getBytes();
        OtpErlangBinary arg2 = new OtpErlangBinary(whiteboardIdBytes);

        byte[] userIdBytes = userId.getBytes();
        OtpErlangBinary arg3 = new OtpErlangBinary(userIdBytes);

        OtpErlangInt arg4 = new OtpErlangInt(isOwner);
        OtpErlangObject[] args = new OtpErlangObject[] { arg1, arg2, arg3, arg4 };
        OtpErlangList argList = new OtpErlangList(args);

        connection.sendRPC("whiteboard", "handle_whiteboard_change", argList);

        return connection.receiveRPC();             // Return the received responses
    }
}
