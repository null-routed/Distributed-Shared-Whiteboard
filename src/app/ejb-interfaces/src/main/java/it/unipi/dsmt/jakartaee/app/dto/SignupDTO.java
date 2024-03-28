package it.unipi.dsmt.jakartaee.app.dto;

import java.io.Serializable;

/***
 * Serializable object for Signup, it carries the information that the server needs to store inside the database.
 */
public class SignupDTO implements Serializable {
    private String username;
    private String password;
    private String email;
    private String name;
    private String surname;


    /***
     * Main constructor for SignupDTO.
     * @param username username of the user that requested the signup
     * @param password password of the user that requested the signup
     * @param email email of the user that requested the signup
     * @param name name of the user that requested the signup
     * @param surname name of the user that requested
     * @param degree degree of the user that requested the signup
     * @param language language of the user that requested the signup
     */
    public SignupDTO(String username, String password, String email, String name, String surname, String degree, String language) {
        this.username = username;
        this.password = password;
        this.email = email;
        this.name = name;
        this.surname = surname;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getSurname() {
        return surname;
    }

    public void setSurname(String surname) {
        this.surname = surname;
    }
}
