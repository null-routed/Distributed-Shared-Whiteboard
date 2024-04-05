package it.unipi.dsmt.jakartaee.app.servlets;

import it.unipi.dsmt.jakartaee.app.dto.AdditionalUserDataDTO;
import it.unipi.dsmt.jakartaee.app.interfaces.UserEJB;
import jakarta.ejb.EJB;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.*;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;


/**
 * Servlet handling POST requests to visualize users profile data
 */
@WebServlet(name = "UserProfileServlet", value = "/profile")
public class UserProfileServlet extends HttpServlet {

    @EJB
    private UserEJB userEJB;

    @Override
    protected void doGet (HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        System.out.println("@UserProfileServlet: called doGet() method");
        String userId = request.getParameter("userId");
        AdditionalUserDataDTO userData = userEJB.getUserDataByUserId(userId);
        // Set the retrieved AdditionalUserDataDTO as an attribute in the request
        request.setAttribute("userData", userData);
        String targetPage = "/WEB-INF/jsp/profile.jsp";
        request.getRequestDispatcher(targetPage).forward(request, response);
    }
}
