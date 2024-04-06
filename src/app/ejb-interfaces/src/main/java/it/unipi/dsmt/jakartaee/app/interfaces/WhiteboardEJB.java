package it.unipi.dsmt.jakartaee.app.interfaces;

import it.unipi.dsmt.jakartaee.app.dto.WhiteboardCreationDTO;
import it.unipi.dsmt.jakartaee.app.dto.MinimalWhiteboardDTO;
import jakarta.ejb.Remote;
import jakarta.validation.constraints.NotNull;


import java.util.List;

@Remote
public interface WhiteboardEJB {
    List<MinimalWhiteboardDTO> searchWhiteboard(@NotNull String name);
    List<MinimalWhiteboardDTO> getAllWhiteboards(@NotNull String userID);
    List<MinimalWhiteboardDTO> getSharedWhiteboards(@NotNull String ID);
    MinimalWhiteboardDTO getWhiteboardByID(@NotNull int whiteboardID);
    int addWhiteboard(@NotNull String ownerID, WhiteboardCreationDTO newWhiteboard);
    boolean deleteWhiteboard(String whiteboardID);
    List<String> getParticipantUsernames(@NotNull int whiteboardID);
    boolean isOwnerOfWhiteboard(@NotNull String userId, @NotNull String whiteboardId);
    boolean removeParticipant(@NotNull String userId, @NotNull String whiteboardId);
}
