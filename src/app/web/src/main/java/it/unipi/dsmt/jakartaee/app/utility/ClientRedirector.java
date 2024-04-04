package it.unipi.dsmt.jakartaee.app.utility;

import jakarta.servlet.ServletException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;


/**
 * Static class providing methods to redirect users' browsers to specific locations.
 */
public class ClientRedirector {

    /**
     * Redirects the user to his whiteboards page.
     * @param request HttpServletRequest instance
     * @param response HttpServletResponse instance
     * @throws IOException if redirection fails
     */
    public static void redirectToMainPage (@NotNull HttpServletRequest request, @NotNull HttpServletResponse response) throws IOException, ServletException {
        System.out.println("@ClientRedirector: forwarding to main page");
        response.sendRedirect(request.getContextPath() + "/homepage");
    }

    /**
     * Redirects the user to the login page.
     * @param request HttpServletRequest instance
     * @param response HttpServletResponse instance
     * @throws IOException if redirection fails
     */
    public static void redirectToLogin (@NotNull HttpServletRequest request,
                                        @NotNull HttpServletResponse response) throws IOException {
        response.sendRedirect(request.getContextPath() + "/");
    }

}
