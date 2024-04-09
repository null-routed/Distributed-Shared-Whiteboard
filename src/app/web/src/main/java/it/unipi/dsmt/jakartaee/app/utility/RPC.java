package it.unipi.dsmt.jakartaee.app.utility;

import com.ericsson.otp.erlang.*;

import java.io.IOException;

public class RPC {

    private static OtpConnection connection = null;

    public static boolean sendErlangWhiteboardUpdateRPC(String command, String whiteboardId, String userId, Boolean isOwner) {
        System.out.println("@RPC: called sendErlangWhiteboardUpdateRPC() method, params=" + command + ", " + whiteboardId + ", " + userId + ", " + isOwner);
        try {
            OtpErlangObject received = executeErlangCommand(command, whiteboardId, userId, isOwner);
            if (received instanceof OtpErlangAtom) {
                String result = ((OtpErlangAtom) received).atomValue();
                System.out.println("The erlang message is: " + result);
                return "ok".equals(result);
            }
        } catch (Exception e) {
            System.out.println("@RPC: failed to execute remote call");
            return false;
        }
        return false;
    }

    public static OtpConnection getConnection() throws IOException, OtpAuthException {
        if (connection == null) {
            OtpSelf self = new OtpSelf("java_client", "XNXTRFGTRHNTNTCISPTB");
            OtpPeer peer = new OtpPeer("erlang_node@localhost");
            connection = self.connect(peer);
        }
        return connection;
    }

    public static OtpErlangObject executeErlangCommand(String command, String whiteboardId, String userId, boolean isOwner)
            throws IOException, OtpAuthException, OtpErlangExit {
        OtpConnection conn = getConnection();           // Connecting to Erlang node
        if (conn == null) {
            System.err.println("@RPC: called executeErlangCommand() method, failed to establish connection.");
            return null;
        }

        OtpErlangString arg1 = new OtpErlangString(command);
        OtpErlangString arg2 = new OtpErlangString(whiteboardId);
        OtpErlangString arg3 = new OtpErlangString(userId);
        OtpErlangBoolean arg4 = new OtpErlangBoolean(isOwner);
        OtpErlangObject[] args = new OtpErlangObject[] { arg1, arg2, arg3, arg4 };
        OtpErlangList argList = new OtpErlangList(args);

        connection.sendRPC("whiteboard_manager", "delete_whiteboard", argList);

        return connection.receiveRPC();             // Return the received responses
    }
}
