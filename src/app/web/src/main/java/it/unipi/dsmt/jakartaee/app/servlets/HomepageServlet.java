package it.unipi.dsmt.jakartaee.app.servlets;

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

import javax.swing.*;
import java.io.IOException;
import java.util.List;


@WebServlet(name = "HomepageServlet", value = "/homepage")
public class HomepageServlet extends HttpServlet {

    @EJB
    private WhiteboardEJB whiteboardEJB;

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        System.out.println("@HomepageServlet: called doGet() method");

        LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
        if (loggedUserDTO == null)
            return;

        String shared =  request.getParameter("shared");
        String searchInput = request.getParameter("search_input");

        List<MinimalWhiteboardDTO> whiteboards;

        if (searchInput != null) {
            request.setAttribute("search_input", searchInput);
            whiteboards = whiteboardEJB.searchWhiteboard(searchInput);
        } else if ((shared != null) && (shared.equals("true"))) {
            whiteboards = whiteboardEJB.getSharedWhiteboards(loggedUserDTO.getId());
            request.setAttribute("shared", shared);
        } else
            whiteboards = whiteboardEJB.getAllWhiteboards(loggedUserDTO.getId());

        String targetPage = "/WEB-INF/jsp/homepage.jsp";

        request.setAttribute("whiteboards", whiteboards);
        request.getRequestDispatcher(targetPage).forward(request, response);
    }
}
