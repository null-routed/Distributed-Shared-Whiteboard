package it.unipi.dsmt.jakartaee.app.servlets.whiteboardManager;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import jakarta.ejb.EJB;
import jakarta.json.JsonObject;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Base64;

@WebServlet (name = "WhiteboardSnapshotServlet", value = "/snapshot_manager")
public class WhiteboardSnapshotServlet extends HttpServlet {

    @EJB
    private WhiteboardEJB whiteboardEJB;

    @Override
    protected void doGet (HttpServletRequest request, HttpServletResponse response) {
        //System.out.println("@WhiteboardSnapshotServlet: called doGet() method");

        String whiteboardID = request.getParameter("whiteboardID");

        byte[] snapshotData = whiteboardEJB.getSnapshotByWhiteboardID(whiteboardID);

        response.setContentType("image/png");

        if (snapshotData != null) {
            try (OutputStream out = response.getOutputStream()) {
//                System.out.println("DOGET(): RETURNING IMAGE TO JSP");
                out.write(snapshotData);
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    @Override
    protected void doPost (HttpServletRequest request, HttpServletResponse response) throws IOException {
        //System.out.println("@WhiteboardSnapshotServlet: called doPost() method");

        ObjectMapper mapper = new ObjectMapper();
        JsonNode jsonNode = mapper.readTree(request.getReader());

        String imageDataURL = jsonNode.get("snapshot").asText();
        String whiteboardID = jsonNode.get("whiteboardID").asText();

        byte[] snapshotDataBytes = Base64.getDecoder().decode(imageDataURL.split(",")[1]);   // Decoding the base64-encoded image data

        boolean snapshotUpdateOutcome = whiteboardEJB.updateWhiteboardSnapshot(snapshotDataBytes, whiteboardID);

        //if (snapshotUpdateOutcome) System.out.println("@WhiteboardSnapshotServlet: successfully updated snapshot for whiteboardID=" + whiteboardID);
        //else System.out.println("@WhiteboardSnapshotServlet: failed to update snapshot for whiteboardID=" + whiteboardID);

        response.setContentType("text/plain");
        response.setCharacterEncoding("UTF-8");
        response.getWriter().write("Image uploaded successfully");
    }
}
