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

    public AdditionalUserDataDTO getUserData(@NotNull String username) {
        System.out.println("UserProfileServlet: called getUserData() method, param=" + username);
        return userEJB.getUserDataByUsername(username);
    }

    @Override
    protected void doGet (HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        System.out.println("@UserProfileServlet: called doGet() method");
        String targetPage = "/WEB-INF/jsp/profile.jsp";
        request.getRequestDispatcher(targetPage).forward(request, response);
    }
}
