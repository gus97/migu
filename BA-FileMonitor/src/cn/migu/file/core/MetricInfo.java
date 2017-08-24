package cn.migu.file.core;

import java.io.Serializable;

public class MetricInfo implements Serializable{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	private Long lastUpdateTime;
	
	/**毫秒*/
	private  Long interval;
	
	private Long fileNum;
	
	private Long recordNum;
	
	public Long getFileNum() {
		return fileNum;
	}
	public void setFileNum(Long fileNum) {
		this.fileNum = fileNum;
	}
	public Long getRecordNum() {
		return recordNum;
	}
	public void setRecordNum(Long recordNum) {
		this.recordNum = recordNum;
	}
	public Long getLastUpdateTime() {
		return lastUpdateTime;
	}
	public void setLastUpdateTime(Long lastUpdateTime) {
		this.lastUpdateTime = lastUpdateTime;
	}
	public Long getInterval() {
		return interval;
	}
	public void setInterval(Long interval) {
		this.interval = interval;
	}
	
	
}
