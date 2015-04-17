package models;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.filter.Filter;
import ch.qos.logback.core.spi.FilterReply;


public class OrdersFilter extends Filter<ILoggingEvent> {

    @Override
    public FilterReply decide(ILoggingEvent event) {
        boolean isAskOrder = event.getFormattedMessage().contains("Ask");
        boolean isBidOrder = event.getFormattedMessage().contains("Bid");
        if ( isAskOrder || isBidOrder) {
            return FilterReply.ACCEPT;
        } else {
            return FilterReply.DENY;
        }
    }
}
