package com.gus.trace.aop;

public final class TraceInfo {

	public final Long traceID;
	public final Long spanID;
	public final Long parentID;
	public final String url;
	// 1=>CS 2=>SR 3=>SS 4=>CR
	public final String status;
	// 毫秒
	public final Long sendTime;
	public final String ip;
	public final String exceptions;

	public TraceInfo(Long traceID, Long spanID, Long parentID, String url, String status, Long sendTime, String ip,
			String exceptions) {
		super();
		this.traceID = traceID;
		this.spanID = spanID;
		this.parentID = parentID;
		this.url = url;
		this.status = status;
		this.sendTime = sendTime;
		this.ip = ip;
		this.exceptions = exceptions;
	}

	@Override
	public String toString() {
		return "TraceInfo [traceID=" + traceID + ", spanID=" + spanID + ", url=" + url + ", status=" + status
				+ ", parentID=" + parentID + ", sendTime=" + sendTime + ", ip=" + ip + ", Exceptions=" + exceptions
				+ "]";
	}

}
