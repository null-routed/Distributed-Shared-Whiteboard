package it.unipi.dsmt.jakartaee.app.interfaces;

import it.unipi.dsmt.jakartaee.app.dto.AdditionalUserDataDTO;
import jakarta.ejb.Remote;
import jakarta.persistence.criteria.CriteriaBuilder;
import org.jetbrains.annotations.NotNull;

import java.util.List;

@Remote
public interface WhiteboardEJB {
    List<String> getParticipantUsernames(int whiteboardID);
}
