package it.unipi.dsmt.jakartaee.app.utility;

import io.jsonwebtoken.*;
import io.jsonwebtoken.security.Keys;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletResponse;

import javax.crypto.SecretKey;
import java.util.Date;

public class JWT{
    // Secret key used to sign the JWT
    private static final String SECRET_KEY = "12345678912345678912345678912345";

    // JWT expiration time (in milliseconds)
    private static final long EXPIRATION_TIME = 86400000; // 24 hours

    // Generate JWT token and set it as a cookie in the response
    public static void generateTokenAndSetCookie(HttpServletResponse response, String username) {
        SecretKey key = Keys.hmacShaKeyFor(SECRET_KEY.getBytes());
        String token = Jwts.builder()
                .subject(username)
                .issuedAt(new Date())
                .expiration(new Date(System.currentTimeMillis() + EXPIRATION_TIME))
                .signWith(key)
                .compact();

        // Create a cookie to store the JWT
        Cookie cookie = new Cookie("jwt", token);
        cookie.setPath("/"); // Specify the cookie's path
        cookie.setHttpOnly(true); // Prevent JavaScript from accessing the cookie
        cookie.setMaxAge((int) (EXPIRATION_TIME / 1000)); // Set cookie expiration time in seconds
        response.addCookie(cookie);
    }

    // Validate and parse JWT token with claim assertions
    public static Claims parseToken(String token, String username) {
        SecretKey key = Keys.hmacShaKeyFor(SECRET_KEY.getBytes());
        try {
            // Configure JwtParserBuilder to use the secret key for signature verification
            JwtParserBuilder builder = Jwts.parser().verifyWith(key).requireSubject(username);
            JwtParser parser = builder.build();

            // Parse the token and extract its claims
            return parser.parseSignedClaims(token).getPayload();
        } catch (JwtException ex) {
            // Handle JwtException if parsing fails
            ex.printStackTrace(); // Example of handling the exception (printing the stack trace)
            return null; // Or handle the exception according to your application's logic
        }
    }
}
