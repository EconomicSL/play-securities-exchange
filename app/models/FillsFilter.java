package models;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.filter.Filter;
import ch.qos.logback.core.spi.FilterReply;



public class FillsFilter extends Filter<ILoggingEvent> {

    @Override
    public FilterReply decide(ILoggingEvent event) {
        boolean isPartialFill = event.getFormattedMessage().contains("PartialFill");
        boolean isTotalFill = event.getFormattedMessage().contains("TotalFill");
        if ( isPartialFill || isTotalFill) {
            return FilterReply.ACCEPT;
        } else {
            return FilterReply.DENY;
        }
    }
}
