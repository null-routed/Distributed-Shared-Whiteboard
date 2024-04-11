package it.unipi.dsmt.jakartaee.app.servlets.whiteboardManager;

import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import jakarta.ejb.EJB;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.util.Base64;

@WebServlet (name = "WhiteboardSnapshotServlet", value = "/save_snapshot")
public class WhiteboardSnapshotServlet extends HttpServlet {

    @EJB
    private WhiteboardEJB whiteboardEJB;

    @Override
    protected void doPost (HttpServletRequest request, HttpServletResponse response) {
        System.out.println("@WhiteboardSnapshotServlet: called doPost() method");

        String snapshot = request.getParameter("snapshot");
        String userID = request.getParameter("userID");
        String whiteboardID = request.getParameter("whiteboardID");

        byte[] snapshotDataBytes = Base64.getDecoder().decode(snapshot);   // Decoding the base64-encoded image data

        boolean snapshotUpdateOutcome = whiteboardEJB.updateWhiteboardSnapshot(userID, snapshotDataBytes, whiteboardID);

        if (snapshotUpdateOutcome) System.out.println("@WhiteboardSnapshotServlet: successfully updated snapshot for whiteboardID=" + whiteboardID);
        else System.out.println("@WhiteboardSnapshotServlet: failed to update snapshot for whiteboardID=" + whiteboardID);
    }
}
