package com.gus.trace.aop;

public final class TraceInfo {

	public final Long traceID;
	public final Long spanID;
	public final String url;
	// 1=>CS 2=>SR 3=>SS 4=>CR
	public final String status;
	public final Long parentID;

	@Override
	public String toString() {
		return "TraceInfo [traceID=" + traceID + ", spanID=" + spanID + ", url=" + url + ", status=" + status
				+ ", parentID=" + parentID + ", sendTime=" + sendTime + "]";
	}

	// 毫秒
	public final Long sendTime;

	public TraceInfo(Long traceID, Long spanID, Long paretID, String url, String status, Long sendTime) {

		this.traceID = traceID;
		this.spanID = spanID;
		this.parentID = paretID;
		this.url = url;
		this.status = status;
		this.sendTime = sendTime;
	}
}
