package it.unipi.dsmt.jakartaee.app.servlets.whiteboardManager;

import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.dto.WhiteboardCreationDTO;
import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import it.unipi.dsmt.jakartaee.app.utility.AccessController;
import it.unipi.dsmt.jakartaee.app.utility.RPC;
import jakarta.annotation.Resource;
import jakarta.ejb.EJB;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.transaction.*;

import java.io.IOException;

@WebServlet(name = "InsertWhiteboardServlet", value = "/insert_whiteboard")
public class InsertWhiteboardServlet extends HttpServlet {

    @EJB
    private WhiteboardEJB whiteboardEJB;

    @Resource
    private UserTransaction userTransaction;

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

        try {               // MySQL + Erlang operations transaction
            userTransaction.begin();

            int mySQLNewWhiteboardID = whiteboardEJB.addWhiteboard(loggedUserDTO.getId(), newWhiteboard);  // MySQL operation

            if (mySQLNewWhiteboardID != -1) {
                boolean erlangInsertOperationOutcome = RPC.sendErlangWhiteboardUpdateRPC(
                        "insert",
                        Integer.toString(mySQLNewWhiteboardID),
                        loggedUserDTO.getUsername(),
                        isReadOnly
                );

                if (erlangInsertOperationOutcome) {
                    userTransaction.commit();
                    response.sendRedirect(request.getContextPath() + "/homepage");
                    return;
                }
            }

            System.out.println("Before rolling back");
            userTransaction.rollback();
            System.out.println("After rolling back");
            response.sendRedirect(request.getContextPath() + "/homepage?insertionFailed=true");
        } catch (Exception e) {
            try {
                userTransaction.rollback();     // rollback if any exception occurs
            } catch (Exception ex) {
                throw new RuntimeException(ex);
            }
            throw new RuntimeException(e);
        }
        response.sendRedirect(request.getContextPath() + "/homepage?insertionFailed=true");
    }
}
