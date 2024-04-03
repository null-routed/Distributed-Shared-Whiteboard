package it.unipi.dsmt.jakartaee.app.servlets;

import it.unipi.dsmt.jakartaee.app.utility.ClientRedirector;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;


/**
 * Servlet to handle logout request.
 */
@WebServlet(name = "LogoutServlet", value = "/logout")
public class LogoutServlet extends HttpServlet {

    private void finalizeLogout(HttpServletRequest request, HttpServletResponse response) throws IOException {
        // Invalidate the session
        request.getSession().invalidate();
        // Redirect to login page
        ClientRedirector.redirectToLogin(request, response);
    }

    @Override
    public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        finalizeLogout(request, response);
    }

    @Override
    public void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        finalizeLogout(request, response);
    }

}