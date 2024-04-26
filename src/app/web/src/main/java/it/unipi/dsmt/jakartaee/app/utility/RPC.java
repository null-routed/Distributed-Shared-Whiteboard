package it.unipi.dsmt.jakartaee.app.utility;

import com.ericsson.otp.erlang.*;

import java.io.IOException;


public class RPC {

    private static OtpConnection connection = null;

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
