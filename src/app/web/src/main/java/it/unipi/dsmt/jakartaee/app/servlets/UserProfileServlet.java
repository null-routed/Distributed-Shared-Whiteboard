package it.unipi.dsmt.jakartaee.app.servlets;

import it.unipi.dsmt.jakartaee.app.dto.AdditionalUserDataDTO;
import it.unipi.dsmt.jakartaee.app.dto.LoggedUserDTO;
import it.unipi.dsmt.jakartaee.app.interfaces.UserEJB;
import it.unipi.dsmt.jakartaee.app.utility.AccessController;
import jakarta.ejb.EJB;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.*;

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
        LoggedUserDTO user = AccessController.getLoggedUserWithRedirect(request, response);
        if(user == null) return;

        AdditionalUserDataDTO userData = userEJB.getUserDataByUserId(user.getId());
        request.setAttribute("userData", userData);
        String targetPage = "/WEB-INF/jsp/profile.jsp";
        request.getRequestDispatcher(targetPage).forward(request, response);
    }
}
