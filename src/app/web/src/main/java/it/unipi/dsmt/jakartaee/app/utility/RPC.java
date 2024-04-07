package it.unipi.dsmt.jakartaee.app.utility;

import com.ericsson.otp.erlang.*;

import java.io.IOException;

public class RPC {

    private static OtpConnection connection = null;

    public static boolean sendErlangWhiteboardUpdateRPC(String command, String whiteboardId, String userId, boolean isOwner) {
        try {
            OtpErlangObject received = getOtpErlangObject(command, whiteboardId, userId, isOwner);
            if (received instanceof OtpErlangAtom) {
                String result = ((OtpErlangAtom) received).atomValue();
                System.out.println("The erlang message is: " + result);
                return "ok".equals(result);
            }
        } catch (Exception e) {
            e.printStackTrace();
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

    public static OtpErlangObject getOtpErlangObject(String command, String whiteboardId, String userId, boolean isOwner)
            throws IOException, OtpAuthException, OtpErlangExit {
        OtpConnection conn = getConnection();
        if (conn == null) {
            System.err.println("Failed to establish connection.");
            return null;
        }

        OtpErlangString arg1 = new OtpErlangString(command);
        OtpErlangString arg2 = new OtpErlangString(whiteboardId);
        OtpErlangString arg3 = new OtpErlangString(userId);
        OtpErlangBoolean arg4 = new OtpErlangBoolean(isOwner);
        OtpErlangObject[] args = new OtpErlangObject[] { arg1, arg2, arg3, arg4 };
        OtpErlangList argList = new OtpErlangList(args);

        connection.sendRPC("whiteboard_manager", "delete_whiteboard", argList);
        return connection.receiveRPC();
    }
}
