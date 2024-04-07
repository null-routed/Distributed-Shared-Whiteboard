package it.unipi.dsmt.jakartaee.app.dto;

import java.io.Serializable;


public class MinimalWhiteboardDTO implements Serializable {
    private int id;
    private String name;
    private String description;
    private String owner;

    public MinimalWhiteboardDTO(int id, String name, String description) {
        this.id = id;
        this.name = name;
        this.description = description;
    }

    public MinimalWhiteboardDTO(int id, String name, String description, String owner) {
        this.id = id;
        this.name = name;
        this.description = description;
        this.owner = owner;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() { return description; }

    public void setDescription(String description) { this.description = description; }

    public String getOwner() {
        return owner;
    }

    public void setOwner(String owner) {
        this.owner = owner;
    }

    @Override
    public String toString() {
         return name;
    }
}
