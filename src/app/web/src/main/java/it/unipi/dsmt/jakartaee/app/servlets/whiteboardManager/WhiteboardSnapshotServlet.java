package it.unipi.dsmt.jakartaee.app.servlets.whiteboardManager;

import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import jakarta.ejb.EJB;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Base64;

@WebServlet (name = "WhiteboardSnapshotServlet", value = "/snapshot_manager")
public class WhiteboardSnapshotServlet extends HttpServlet {

    @EJB
    private WhiteboardEJB whiteboardEJB;

    @Override
    protected void doGet (HttpServletRequest request, HttpServletResponse response) {
        System.out.println("@WhiteboardSnapshotServlet: called doGet() method");

        String whiteboardID = request.getParameter("whiteboardID");
        String userID = request.getParameter("userID");

        byte[] snapshotData = whiteboardEJB.getSnapshotByWhiteboardID(whiteboardID, userID);

        response.setContentType("image/png");

        if (snapshotData != null) {
            try (OutputStream out = response.getOutputStream()) {
                out.write(snapshotData);
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    @Override
    protected void doPost (HttpServletRequest request, HttpServletResponse response) throws IOException {
        System.out.println("@WhiteboardSnapshotServlet: called doPost() method");

        String imageDataString = request.getParameter("snapshot");
        String userID = request.getParameter("userID");
        String whiteboardID = request.getParameter("whiteboardID");

        byte[] snapshotDataBytes = Base64.getDecoder().decode(imageDataString.split(",")[1]);   // Decoding the base64-encoded image data

        boolean snapshotUpdateOutcome = whiteboardEJB.updateWhiteboardSnapshot(userID, snapshotDataBytes, whiteboardID);

        if (snapshotUpdateOutcome) System.out.println("@WhiteboardSnapshotServlet: successfully updated snapshot for whiteboardID=" + whiteboardID);
        else System.out.println("@WhiteboardSnapshotServlet: failed to update snapshot for whiteboardID=" + whiteboardID);

        response.setContentType("text/plain");
        response.setCharacterEncoding("UTF-8");
        response.getWriter().write("Image uploaded successfully");
    }
}
