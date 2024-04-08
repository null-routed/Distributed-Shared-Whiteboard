package it.unipi.dsmt.jakartaee.app.servlets.whiteboardManager;

import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.dto.WhiteboardCreationDTO;
import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import it.unipi.dsmt.jakartaee.app.utility.AccessController;
import it.unipi.dsmt.jakartaee.app.utility.RPC;
import jakarta.ejb.EJB;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;

@WebServlet(name = "InsertWhiteboardServlet", value = "/insert_whiteboard")
public class InsertWhiteboardServlet extends HttpServlet {

    @EJB
    private WhiteboardEJB whiteboardEJB;

    @Override
    protected void doPost (HttpServletRequest request, HttpServletResponse response) throws IOException {
        System.out.println("@InsertWhiteboardServlet: called doPost() method");

        LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
        if (loggedUserDTO == null)
            return;

        String whiteboardName = request.getParameter("whiteboardName");
        String whiteboardDescription = request.getParameter("whiteboardDescription");
        String readOnlyParam = request.getParameter("readOnly");

        boolean isReadOnly = "on".equals(readOnlyParam);        // Converting "readOnlyParam" to boolean

        WhiteboardCreationDTO newWhiteboard = new WhiteboardCreationDTO(
                whiteboardName,
                whiteboardDescription,
                isReadOnly
        );

        // Erlang communication
        try {
            int newWhiteboardId = whiteboardEJB.addWhiteboard(loggedUserDTO.getId(), newWhiteboard);
            if (newWhiteboardId != -1) {                 // Proceed with Erlang synchronization only if the operation was ok in MySQL DB
                boolean erlangOperationOutcome = RPC.sendErlangWhiteboardUpdateRPC(
                        "insert",
                        Integer.toString(newWhiteboardId),
                        loggedUserDTO.getUsername(),
                        isReadOnly
                );
                if (erlangOperationOutcome)
                    response.sendRedirect(request.getContextPath() + "/homepage"); // Redirect to the homepage
            } else
                response.sendRedirect(request.getContextPath() + "/homepage?insertionFailed=true");
        } catch (RuntimeException e) {
            throw new RuntimeException(e);
        }
    }
}
