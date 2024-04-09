package it.unipi.dsmt.jakartaee.app.servlets.whiteboardManager;

import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import it.unipi.dsmt.jakartaee.app.utility.AccessController;
import it.unipi.dsmt.jakartaee.app.utility.RPC;
import jakarta.annotation.Resource;
import jakarta.ejb.EJB;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.transaction.UserTransaction;

import java.io.IOException;


@WebServlet (name = "DeleteWhiteboardServlet", value = "/delete_whiteboard")
public class DeleteWhiteboardServlet extends HttpServlet {

    @EJB
    private WhiteboardEJB whiteboardEJB;

    @Resource
    private UserTransaction userTransaction;

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        System.out.println("@DeleteWhiteboardServlet: called doPost() method");

        LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
        if (loggedUserDTO == null)
            return;

        String whiteboardIdToDelete = request.getParameter("whiteboardIdToDelete");

        if (whiteboardIdToDelete != null) {

            boolean clientIsOwner = whiteboardEJB.isWhiteboardOwner(loggedUserDTO.getId(), whiteboardIdToDelete);
            if (clientIsOwner) {        // Erlang + MySQL operations transaction if the requester is the whiteboard owner

                try {
                    userTransaction.begin();

                    boolean mySQLDeleteOperationOutcome = whiteboardEJB.deleteWhiteboard(whiteboardIdToDelete);

                    if (mySQLDeleteOperationOutcome) {
                        boolean erlangDeleteOperationOutcome = RPC.sendErlangWhiteboardUpdateRPC(
                                "delete",
                                whiteboardIdToDelete,
                                null,       // not needed Erlang side
                                null
                        );

                        if (erlangDeleteOperationOutcome) {
                            userTransaction.commit();
                            response.sendRedirect(request.getContextPath() + "/homepage");
                        } else {
                            userTransaction.rollback();     // rollback if Erlang operation fails
                            response.sendRedirect(request.getContextPath() + "/homepage?deletionFailed=true");
                        }
                    } else {
                        userTransaction.rollback();         // rollback if MySQL operation fails
                        response.sendRedirect(request.getContextPath() + "/homepage?deletionFailed=true");
                    }
                } catch (Exception e) {
                    try {
                        userTransaction.rollback();     // rollback if any exception occurs
                    } catch (Exception ex) {
                        throw new RuntimeException(ex);
                    }
                    throw new RuntimeException(e);
                }

                return;     // Return to prevent further processing, the op has concluded for the owner of the whiteboard
            }

            // If here, the user requesting a DELETE is not the owner -> remove him from the whiteboard participants
            try{
                userTransaction.begin();

                boolean mySQLParticipantRemovalOutcome = whiteboardEJB.removeParticipant(loggedUserDTO.getId(), whiteboardIdToDelete);

                if (mySQLParticipantRemovalOutcome) {
                    boolean erlangParticipantRemovalOutcome = RPC.sendErlangWhiteboardUpdateRPC(
                            "delete",
                            whiteboardIdToDelete,
                            loggedUserDTO.getUsername(),        // user to be removed from the whiteboard participants list
                            null            // not needed Erlang side
                    );

                    if (erlangParticipantRemovalOutcome) {
                        userTransaction.commit();
                        response.sendRedirect(request.getContextPath() + "/homepage");
                    } else {
                        userTransaction.rollback();     // rollback if Erlang operation failed
                        response.sendRedirect(request.getContextPath() + "/homepage?deletionFailed=true");
                    }
                } else {
                    userTransaction.rollback();     // rollback if MySQL operation failed
                    response.sendRedirect(request.getContextPath() + "/homepage?deletionFailed=true");
                }
            } catch (Exception e) {
                try {
                    userTransaction.rollback();     // rollback if any exception occurs
                } catch (Exception ex) {
                    // throw new RuntimeException(ex);
                    response.sendRedirect(request.getContextPath() + "/homepage?insertionFailed=true");
                }
                // throw new RuntimeException(e);
                response.sendRedirect(request.getContextPath() + "/homepage?insertionFailed=true");
            }
        }
    }
}
