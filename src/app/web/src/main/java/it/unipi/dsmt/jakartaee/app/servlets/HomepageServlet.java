package it.unipi.dsmt.jakartaee.app.servlets;

import com.ericsson.otp.erlang.*;
import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO;
import it.unipi.dsmt.jakartaee.app.dto.WhiteboardCreationDTO;
import it.unipi.dsmt.jakartaee.app.interfaces.WhiteboardEJB;
import it.unipi.dsmt.jakartaee.app.utility.AccessController;
import it.unipi.dsmt.jakartaee.app.utility.RPC;
import jakarta.ejb.EJB;
import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;
import java.util.List;


@WebServlet(name = "HomepageServlet", value = "/homepage")
public class HomepageServlet extends HttpServlet {

    @EJB
    private WhiteboardEJB whiteboardEJB;

    private void handleRequest(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

        List<MinimalWhiteboardDTO> whiteboards;

        LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
        if (loggedUserDTO == null)
            return;

        String shared =  request.getParameter("shared");
        String searchInput = request.getParameter("search_input");

        if (searchInput != null) {
            request.setAttribute("search_input", searchInput);
            whiteboards = whiteboardEJB.searchWhiteboard(searchInput);
        }
        else if ((shared != null) && (shared.equals("true"))) {
            whiteboards = whiteboardEJB.getSharedWhiteboards(loggedUserDTO.getId());
            request.setAttribute("shared", shared);
        } else
            whiteboards = whiteboardEJB.getAllWhiteboards(loggedUserDTO.getId());

        request.setAttribute("whiteboards", whiteboards);

        String relativePath = "/WEB-INF/jsp/homepage.jsp";

        RequestDispatcher requestDispatcher = request.getRequestDispatcher(relativePath);
        requestDispatcher.forward(request, response);
    }

    @Override
    protected void doPost (HttpServletRequest request, HttpServletResponse response) throws IOException {
        System.out.println("@HomepageServlet: called doPost() method");

        LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
        if (loggedUserDTO == null)
            return;

        // Check if the request is for deleting a whiteboard
        String whiteboardIdToDelete = request.getParameter("whiteboardIdToDelete");
        if (whiteboardIdToDelete != null) {
            // Call a method to delete the whiteboard
            if(whiteboardEJB.isWhiteboardOwner(loggedUserDTO.getId(), whiteboardIdToDelete)) {
                if(whiteboardEJB.deleteWhiteboard(whiteboardIdToDelete)) {
                    if (RPC.sendErlangWhiteboardUpdateRPC("delete", whiteboardIdToDelete, loggedUserDTO.getId(), true))
                        response.sendRedirect(request.getContextPath() + "/homepage");
                    else
                        response.sendRedirect(request.getContextPath() + "/homepage?deletionFailed=true");
                } else
                    response.sendRedirect(request.getContextPath() + "/homepage?deletionFailed=true");
            }
            else {
                if (whiteboardEJB.removeParticipant(loggedUserDTO.getId(), whiteboardIdToDelete)) {
                    if (RPC.sendErlangWhiteboardUpdateRPC("delete" ,whiteboardIdToDelete, loggedUserDTO.getId(), false))
                        response.sendRedirect(request.getContextPath() + "/homepage");
                    else
                        response.sendRedirect(request.getContextPath() + "/homepage?deletionFailed=true");
                } else
                    response.sendRedirect(request.getContextPath() + "/homepage?deletionFailed=true");
            }
            return; // Return to prevent further processing
        }

        // Retrieving parameters from the request
        String whiteboardName = request.getParameter("whiteboardName");
        String whiteboardDescription = request.getParameter("whiteboardDescription");
        String readOnlyParam = request.getParameter("readOnly");

        // Converting "readOnlyParam" to boolean
        boolean isReadOnly = "on".equals(readOnlyParam);

        WhiteboardCreationDTO newWhiteboard = new WhiteboardCreationDTO(
                whiteboardName,
                whiteboardDescription,
                isReadOnly
        );

        try {
            int newWhiteboardId = whiteboardEJB.addWhiteboard(loggedUserDTO.getId(), newWhiteboard);
            if(newWhiteboardId != -1) {
                // TODO you can also pass the DTO by serializing it
                if(RPC.sendErlangWhiteboardUpdateRPC("insert", Integer.toString(newWhiteboardId), loggedUserDTO.getId(), isReadOnly))
                    response.sendRedirect(request.getContextPath() + "/homepage"); // Redirect to the homepage
            } else {
                System.out.println("@HomepageServlet: whiteboard insertion failed");
                response.sendRedirect(request.getContextPath() + "/homepage?insertionFailed=true");
            }
        } catch (RuntimeException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        handleRequest(request, response);
    }
}
