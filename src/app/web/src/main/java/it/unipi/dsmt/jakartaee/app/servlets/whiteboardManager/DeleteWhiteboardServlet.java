package it.unipi.dsmt.jakartaee.app.servlets.whiteboardManager;

import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import it.unipi.dsmt.jakartaee.app.utility.AccessController;
import it.unipi.dsmt.jakartaee.app.utility.RPC;
import jakarta.ejb.EJB;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;


@WebServlet (name = "DeleteWhiteboardServlet", value = "/delete_whiteboard")
public class DeleteWhiteboardServlet extends HttpServlet {

    @EJB
    private WhiteboardEJB whiteboardEJB;

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        System.out.println("@DeleteWhiteboardServlet: called doPost() method");

        LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
        if (loggedUserDTO == null)
            return;

        String whiteboardIdToDelete = request.getParameter("whiteboardIdToDelete");

        if (whiteboardIdToDelete != null) {
            boolean clientIsOwner = whiteboardEJB.isWhiteboardOwner(loggedUserDTO.getId(), whiteboardIdToDelete);
            if (clientIsOwner) {
                boolean deleteOperationOutcome = whiteboardEJB.deleteWhiteboard(whiteboardIdToDelete);
                if (deleteOperationOutcome) {           // TODO: transaction, if erlang fails the also SQL has to be rolled back
                    boolean erlangDeleteOperationOutcome = RPC.sendErlangWhiteboardUpdateRPC(
                            "delete",
                            whiteboardIdToDelete,
                            null,               // not needed Erlang side
                            null
                    );
                    if (erlangDeleteOperationOutcome)       // TODO: check, upon deleting you get redirected to a blank page
                        response.sendRedirect(request.getContextPath() + "/homepage");
                    else
                        response.sendRedirect(request.getContextPath() + "/homepage?deletionFailed=true");
                } else
                    response.sendRedirect(request.getContextPath() + "/homepage?deletionFailed=true");

                return;         // Return to prevent further processing, the op has concluded for the owner of the whiteboard
            }

            // If here, the user requesting a DELETE is not the owner -> remove him from the whiteboard participants
            boolean participantRemovalOutcome = whiteboardEJB.removeParticipant(loggedUserDTO.getId(), whiteboardIdToDelete);
            try {       // TODO: transaction, if erlang fails the also SQL has to be rolled back
                if (participantRemovalOutcome) {
                    boolean erlangDeleteOperationOutcome = RPC.sendErlangWhiteboardUpdateRPC(
                            "delete",
                            whiteboardIdToDelete,
                            loggedUserDTO.getUsername(),            // user to be removed from the whiteboard participants
                            null
                    );
                    if (erlangDeleteOperationOutcome)
                        response.sendRedirect(request.getContextPath() + "/homepage");
                    else
                        response.sendRedirect(request.getContextPath() + "homepage?deletionFailed=true");
                } else
                    response.sendRedirect(request.getContextPath() + "/homepage?deletionFailed=true");
            } catch (RuntimeException e) {
                throw new RuntimeException(e);
            }
        }
    }
}
